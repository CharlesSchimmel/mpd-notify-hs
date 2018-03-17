#!/usr/bin/env runhaskell

module Lib
  ( libMain
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception as Exc
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Data.String.Utils
import Debug.Trace
import Libnotify
import Network.MPD
import Safe as Sf
import System.Directory
import System.Environment
import System.FilePath
import System.Path.NameManip
import System.Process

import PathHelpers
import Options
import Images

changeWall :: String -> FilePath -> IO String
changeWall "center" image = readProcess "feh" ["--bg-center",image] ""
changeWall "fill" image = readProcess "feh" ["--bg-fill",image] ""
changeWall "max" image = readProcess "feh" ["--bg-max",image] ""
changeWall "scale" image = readProcess "feh" ["--bg-scale",image] ""
changeWall "tile" image = readProcess "feh" ["--bg-tile",image] ""
changeWall "smart" image = readProcess "feh" ["--bg-tile",image] ""
changeWall _ image = changeWall "tile" image

songNotif :: Maybe Notification -> IO Notification
songNotif token = do
  Right(Just(song)) <- ($ currentSong) =<< mpdCon' -- this is Maybe poor taste, but the way it's used there must be a Right, Just result
  cover <- findLargestArt =<< (</> (toString $ sgFilePath song)) <$> libraryPath
  let coverpath = filepath <$> cover
  when (isJust cover) $ (void  ((\(Just x) -> changeWall "tile" x) coverpath))
  let notif = maybe (notifTemplate song <> icon "") (\x -> notifTemplate song <> icon x) coverpath
  maybe (display notif) (\x -> display $ reuse x <> notif) token

songAttr :: Song -> Metadata -> String
songAttr song meta = maybe "<unkown>" (toString . head) (sgGetTag meta song)

notifTemplate :: Song -> Libnotify.Mod Notification
notifTemplate song = summary (title ++ " - " ++ artist) <> body album
  where [artist,title,album] = (songAttr song) <$> [Artist,Title,Album]

libraryPath :: IO String
libraryPath = absolutize =<< libPath <$> getOptions

-- if we already have a notification token, use it and clear the body (this has the pleasant side effect of also reusing the album art which is a nice touch.) Otherwise create one
statusNotif :: Show a=> a -> Maybe Notification -> IO Notification
statusNotif state token = maybe (display summ) (\x -> display (reuse x <> summ <> body "")) token
  where summ = summary $ show $ state

mpdCon :: String -> Int -> MPD a -> IO (Response a)
mpdCon host port = withMPD_ (Just host) (Just $ show port)

mpdCon' = mpdBuilder <$> getOptions
  where mpdBuilder (Opti _ port host) = withMPD_ (Just host) (Just $ show port)

waitForChange :: MonadMPD f=>(f Status -> IO (Either a Status)) -> Maybe Notification -> IO Notification
waitForChange mpdConnection notif = do
  new <- curStat <$> ( mpdConnection $ idle [PlayerS] *> status ) -- wait for player change, and get status
  if (new /= Playing)
     then statusNotif new notif
     else songNotif notif
    where curStat = either (\_ -> Stopped) stState

mainLoop :: Maybe Notification -> Opti -> IO ()
mainLoop notif args = do
  let mpdConnection = mpdCon (host args) (port args)
  mpdStatus <- mpdConnection $ status
  if (isRight mpdStatus)
     then waitForChange mpdConnection notif >>= (\x -> mainLoop (Just x) args)
     else putStrLn "Cannot connect to MPD. Check your host and port and make sure MPD is running."

libMain :: IO()
libMain = do
  args <- getOptions
  libraryValid <- doesPathExist =<< (absolutize $ libPath args)
  when (not libraryValid) (putStrLn "Library path not found. Album art won't work.")
  mainLoop Nothing args
