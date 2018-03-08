#!/usr/bin/env runhaskell

module Lib
  ( mainLoop
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Exception as Exc
import Data.Either
import Data.String.Utils
import Libnotify
import Network.MPD
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Safe as Sf
-- import Options.Generic

data Cover = Cover { width :: Int, path :: String} deriving (Show, Ord, Eq)

-- data Options = Options { host :: String, port :: Int } deriving (Generic, Show)
-- instance ParseRecord Options

-- buildCovers justImages = do
--   coverList <- map (

-- getCommonColor image = do
--   readProcess "convert" [image,"-colors","2","-depth","8","-unique-colors","format
--

libraryDir = Sf.headMay <$> getArgs
libraryDirExists = libraryDir >>= (maybe (return False) doesPathExist)

-- check tags for desired metadata. if exists, convert to String
songAttr::Either a (Maybe Song) -> Metadata -> String
songAttr song meta = maybe "<unkown>" (toString . head) (maybe Nothing (sgGetTag meta) (unwrap song))
  where unwrap = either (\x -> Nothing) id

-- Maybe not best practice but MPD will (should) always return something for the filepath
getDir::Either a (Maybe Song) -> FilePath
getDir = either (\x -> "") (maybe "" (\z -> toString $ sgFilePath z))

-- get files also in the same folder as the file
getNeighbors path = do
  doesExist <- libraryDirExists
  if doesExist then (do
               basedir <- maybe "" (</> takeDirectory path) <$> libraryDir
               neighbors <- listDirectory basedir
               return $ (basedir </>) <$> neighbors) else (return [])

findCovers = filter (endswith ".jpg")

-- eventually we'll find the largest of the images...
getWidth file = do
  readProcess "identify" (["-format","%W",file]) ""

-- eventually we'll find the largest of the images...
padCover images 
  | null images = Nothing
  | otherwise = Just (maximum $ images)

notifArt title artist album cover = display (summary (artist ++ " - " ++ title) <> body album <> icon cover)

notifPlain title artist album = display (summary (artist ++ " - " ++ title) <> body album)

songNotif = do
  cSong <- withMPD $ currentSong

  let [curArtist,curTitle,curAlbum] = (songAttr cSong) <$> [Artist,Title,Album]

  neighbors <- getNeighbors (getDir cSong)
  let cover = padCover $ findCovers neighbors

  maybe (return ("No cover found") :: IO String) (\x -> readProcess "feh" ["--bg-tile",x] "") cover
  maybe (notifPlain curTitle curArtist curAlbum) (notifArt curTitle curArtist curAlbum) cover

-- unwrap the either, basically. Will only be left if not running.
curStat = either (\x -> Stopped) (stState)

statusNotif newState = do
  display (summary $ show $ curStat newState)

-- left: play state has changed. right: song has changed
playerChange oldState newState 
  | curStat oldState == curStat newState = songNotif
  | otherwise =  statusNotif newState

subLoop = do
  oldState <- withMPD $ status
  isIdle <- withMPD $ idle [PlayerS]
  newState <- withMPD $ status

  -- left: mpd not running; pass. right: something changed
  when (isRight isIdle) (void $ playerChange oldState newState)
  subLoop

mainLoop = do
  libDirExists <- libraryDirExists
  when (not libDirExists) (putStrLn "Requires music library directory as argument. Album art won't work.")
  subLoop
