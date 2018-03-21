#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( libMain
  ) where

import Codec.Picture
import Control.Exception
import Control.Monad
import Data.Either
import Data.Maybe
import Libnotify
import Network.MPD
import System.Directory
import System.FilePath
import System.Process

import PathHelpers
import Options
import Images

changeWall :: String -> FilePath -> IO String
changeWall "center" path = readProcess "feh" ["--bg-center",path] ""
changeWall "fill" path = readProcess "feh" ["--bg-fill",path] ""
changeWall "max" path = readProcess "feh" ["--bg-max",path] ""
changeWall "scale" path = readProcess "feh" ["--bg-scale",path] ""
changeWall "tile" path = readProcess "feh" ["--bg-tile",path] ""
changeWall "smart" path = readProcess "feh" ["--bg-tile",path] ""
changeWall _ path = changeWall "tile" path

songNotif :: Maybe Notification -> IO Notification
songNotif token = do
  Right(Just song) <- ($ currentSong) =<< mpdCon' -- this is Maybe poor taste, but the way it's used there must be a Right, Just result
  foo <- findLargestArt =<< (</> (toString $ sgFilePath song)) <$> libraryPath -- Maybe Cover
  let coverpath = coverPath <$> foo                                            -- Maybe FilePath
  -- maybe (putStrLn "bar1") updateWall foo
  when (isJust coverpath) $ void (changeWall "tile" $ fromJust coverpath)
  let notif = maybe (notifTemplate song <> icon "") (\x -> notifTemplate song <> icon x) coverpath
  maybe (display notif) (\x -> display $ reuse x <> notif) token

-- updateWall :: Cover -> IO ()
updateWall cover = do
  writePng "/tmp/mpd-notify-cover.png" $ matteCover cover (1920,1080)
  changeWall "center" "/tmp/mpd-notify-cover.png"
  return ()

songAttr :: Song -> Metadata -> String
songAttr song meta = maybe "<unkown>" (toString . head) (sgGetTag meta song)

notifTemplate :: Song -> Libnotify.Mod Notification
notifTemplate song = summary (title ++ " - " ++ artist) <> body album
  where [artist,title,album] = songAttr song <$> [Artist,Title,Album]

-- if we already have a notification token, use it and clear the body.
statusNotif :: Show a=> a -> Maybe Notification -> IO Notification
statusNotif state = maybe (display summ) (\x -> display (reuse x <> summ <> body ""))
  where summ = summary $ show state

mpdCon :: String -> Int -> MPD a -> IO (Response a)
mpdCon h p = withMPD_ (Just h) (Just $ show p)

-- Sort of a factory for building mpd connections
mpdCon' :: IO (MPD a -> IO (Response a))
mpdCon' = mpdBuilder <$> getOptions
  where mpdBuilder opt = withMPD_ (Just $ host opt) (Just $ show $ port opt)

waitForChange :: MonadMPD f=>(f Status -> IO (Either a Status)) -> Maybe Notification -> IO Notification
waitForChange mpdConnection notif = do
  new <- curStat <$> mpdConnection  (idle [PlayerS] *> status) -- wait for player change, and get status
  if new /= Playing
     then statusNotif new notif
     else songNotif notif
    where curStat = either (const Stopped) stState

mainLoop :: Maybe Notification -> Opti -> IO ()
mainLoop notif args = do
  let mpdConnection = mpdCon (host args) (port args)
  mpdStatus <- mpdConnection status
  if isRight mpdStatus
     then waitForChange mpdConnection notif >>= (\x -> mainLoop (Just x) args)
     else putStrLn "Cannot connect to MPD. Check your host and port and make sure MPD is running."

-- "Do once"
libMain :: IO()
libMain = do
  args <- getOptions
  libraryValid <- doesPathExist =<< absolutize (libPath args)
  unless libraryValid (putStrLn "Library path not found. Album art won't work.")
  mainLoop Nothing args
