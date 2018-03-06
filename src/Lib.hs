#!/usr/bin/env runhaskell

module Lib
  ( mainLoop
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.MPD
import Data.Map.Strict as Map hiding (null)
import System.Directory
import System.FilePath.Posix
import System.Process
import Data.String.Utils
import Control.Concurrent
import Control.Monad
import Libnotify
import System.Exit
import Data.Either
-- import Options.Generic

data Cover = Cover { width :: Int, path :: String} deriving (Show, Ord, Eq)

-- data Options = Options { host :: String, port :: Int } deriving (Generic, Show)
-- instance ParseRecord Options

-- buildCovers justImages = do
--   coverList <- map (

-- getCommonColor image = do
--   readProcess "convert" [image,"-colors","2","-depth","8","-unique-colors","format

songAttr::Metadata -> Either a (Maybe Song) -> String
songAttr meta song = maybe "<unkown>" (toString . head) (maybe Nothing (sgGetTag meta) (unwrap song))
  where unwrap = either (\x -> Nothing) id

-- Maybe not best practice but MPD will (should) always return something for the filepath
getDir::Either a (Maybe Song) -> FilePath
getDir = either (\x -> "") (maybe "" (\z -> toString $ sgFilePath z))

-- get files also in the same folder as the song
getNeighbors curSong = do
  let basedir = (++ "/") $ takeDirectory $ "/home/elpfen/mus/" ++ getDir curSong 
  neighbors <- getDirectoryContents basedir
  return $ Prelude.map (basedir++) neighbors

findCovers = Prelude.filter (endswith ".jpg")

getWidth file = do
  readProcess "identify" (["-format","%W",file]) ""

-- to us, whether it's not responding or paused/stopped doesn't matter
curStat::Either a Status -> State
curStat = either (\x -> Paused) (stState)

padCover images 
  | null images = Nothing
  | otherwise = Just (maximum $ images)

notifArt title artist album cover = do
  display (summary (artist ++ " - " ++ title) <> body album <> icon cover)

notifPlain title artist album = do
  display (summary (artist ++ " - " ++ title) <> body album)

playingChanged :: Response(Status) -> Response(Status) -> Bool
playingChanged old new = (curStat old) /= (curStat new)

-- mpdDetected::Response(Status) -> IO ()
mpdDetected = do

  cOldState <- withMPD $ status

  withMPD $ idle [PlayerS]

  cSong <- withMPD $ currentSong
  cNewState <- withMPD $ status

  let curArtist = songAttr Artist cSong
      curTitle = songAttr Title cSong
      curAlbum = songAttr Album cSong

  neighbors <- getNeighbors cSong
  let covers = findCovers neighbors

  let cover = padCover covers

  maybe (return ("No cover found") :: IO String) (\x -> readProcess "feh" ["--bg-tile",x] "") cover

  when (not $ playingChanged cOldState cNewState) . void $ maybe (notifPlain curTitle curArtist curAlbum) (notifArt curTitle curArtist curAlbum) cover

  when (playingChanged cOldState cNewState) . void $ display (summary $ show $ curStat cNewState)

  return ()


mainLoop = do
  -- opts <- getRecord "MPD Notify"
  -- putStrLn opts

  cOldState <- withMPD $ status
  when (not (isLeft cOldState)) . void $ mpdDetected

  mainLoop
