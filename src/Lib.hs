#!/usr/bin/env runhaskell
module Lib
  ( mainLoop
  ) where

{-# LANGUAGE OverloadedStrings #-}
import Network.MPD
import Data.Map.Strict as Map
import System.Directory
import System.FilePath.Posix
import System.Process
import Data.String.Utils
import Control.Concurrent
import Control.Monad
import Libnotify
import System.Exit


data Cover = Cover {width :: Int, path :: String} deriving (Show, Ord, Eq)

songAttr::Response(Maybe Song) -> Metadata -> String
songAttr r t = either (\x -> "") (maybe "" (\z -> toString $ head $ (! t) $ sgTags z)) r

-- I think this smells? I don't think it's possible for MPD to not have a filepath for a song...
getDir = either (\x -> "") (maybe "" (\z -> toString $ sgFilePath z))

-- get files also in the same folder as the song
getNeighbors curSong = do
  let basedir = (++ "/") $ takeDirectory $ "/home/elpfen/mus/" ++ getDir curSong 
  neighbors <- getDirectoryContents basedir
  return $ Prelude.map (basedir++) neighbors

findCovers = Prelude.filter (endswith ".jpg")

-- buildCovers justImages = do
--   coverList <- map (

-- getCommonColor image = do
--   readProcess "convert" [image,"-colors","2","-depth","8","-unique-colors","format

getWidth file = do
  readProcess "identify" (["-format","%W",file]) ""

curStat = either (\x -> Paused) (stState)

padCover neighbors 
  | (length $ justImages) == 0 = Nothing
  | otherwise = Just (maximum $ justImages)
  where justImages = findCovers neighbors

notifArt title artist album cover = do
  display (summary (artist ++ " - " ++ title) <> body album <> icon cover)

notifPlain title artist album = do
  display (summary (artist ++ " - " ++ title) <> body album)

playingChanged :: Response(Status) -> Response(Status) -> Bool
playingChanged old new = (curStat old) /= (curStat new)


mainLoop = do
  cOldState <- withMPD $ status

  withMPD $ idle [PlayerS]

  cSong <- withMPD $ currentSong
  cNewState <- withMPD $ status

  let curArtist = songAttr cSong Artist
      curTitle = songAttr cSong Title
      curAlbum = songAttr cSong Album

  neighbors <- getNeighbors cSong

  let cover = padCover neighbors

  maybe (return ("No cover found") :: IO String) (\x -> readProcess "feh" ["--bg-tile",x] "") cover

  when (not $ playingChanged cOldState cNewState) . void $ maybe (notifPlain curTitle curArtist curAlbum) (notifArt curTitle curArtist curAlbum) cover

  when (playingChanged cOldState cNewState) . void $ display (summary $ show $ curStat cNewState)

  mainLoop
