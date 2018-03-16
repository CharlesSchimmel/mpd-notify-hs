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
import Options.Applicative
import Safe as Sf
import System.Directory
import System.Environment
import System.FilePath
import System.Path.NameManip
import System.Process

import PathHelpers
import Options

-- eventually we'll find the largest of the images...
getWidth file = do
  readProcess "identify" (["-format","%W",file]) ""

changeWall :: String -> FilePath -> IO String
changeWall "tile" image = readProcess "feh" ["--bg-tile",image] ""
changeWall "center" image = readProcess "feh" ["--bg-center",image] ""
changeWall "stretch" image = readProcess "feh" ["--bg-maimage",image] ""
changeWall "smart" image = readProcess "feh" ["--bg-tile",image] ""
changeWall _ image = changeWall "tile" image

songNotif :: Maybe Notification -> IO Notification
songNotif token = do
  Right(Just(song)) <- ($ currentSong) =<< mpdCon'
  cover <- findArt =<< (</> (toString $ sgFilePath song)) <$> libraryPath
  updateArt cover
  let notif = maybe (notifTemplate song <> icon "") (\x -> notifTemplate song <> icon x) cover
  maybe (display notif) (\x -> display $ reuse x <> notif) token

-- check tags for desired metadata. if exists, convert to String
songAttr :: Song -> Metadata -> String
songAttr song meta = maybe "<unkown>" (toString . head) (sgGetTag meta song)

notifTemplate :: Song -> Libnotify.Mod Notification
notifTemplate song = summary (title ++ " - " ++ artist) <> body album
  where [artist,title,album] = (songAttr song) <$> [Artist,Title,Album]

updateArt :: Maybe FilePath -> IO String
updateArt cover = maybe (return ("No cover found") :: IO String) (changeWall "tile") cover

statusNotif :: Show a=> a -> Maybe Notification -> IO Notification
statusNotif state token = maybe (display (summary $ show $ state)) (\x -> display (reuse x <> (summary $ show $ state) <> body "")) token

mpdCon :: String -> Int -> MPD a -> IO (Response a)
mpdCon host port = withMPD_ (Just host) (Just $ show port)

mpdCon' = mpdBuilder <$> execParser opts 
  where mpdBuilder (Opti _ port host) = withMPD_ (Just host) (Just $ show port)

libraryPath :: IO String
libraryPath = absolutize =<< libPath <$> execParser opts

waitForChange :: MonadMPD f=>(f Status -> IO (Either a Status)) -> Maybe Notification -> IO Notification
waitForChange mpdConnection notif = do
  new <- curStat <$> ( mpdConnection $ idle [PlayerS] *> status )
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
  args <- execParser opts
  libraryValid <- doesPathExist =<< (absolutize $ libPath args)
  when (not libraryValid) (putStrLn "Library path not found. Album art won't work.")
  mainLoop Nothing args
