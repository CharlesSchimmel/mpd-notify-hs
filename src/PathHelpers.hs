{-# LANGUAGE OverloadedStrings #-}

module PathHelpers
  ( 
  absolutize
, findArt
, findLargestArt
, getNeighbors
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import System.Directory
import System.FilePath
import System.Path.NameManip

import Images

absolutize :: String -> IO String
absolutize apath
  | "~" `isPrefixOf` apath = do
    homePath <- getHomeDirectory
    return $ normalise $ addTrailingPathSeparator homePath ++ tail apath
  | otherwise = do
    pathMaybewithDots <- absolute_path apath
    return $ fromJust $ guess_dotdot pathMaybewithDots

-- get files also in the same folder as the file
-- TODO: convert to not care whether the given FilePath is a file or a dir
getNeighbors :: String -> IO [FilePath]
getNeighbors file = do
  doesExist <- doesPathExist =<< absolutize file
  if doesExist then (do
               basedir <- takeDirectory <$> absolutize file
               neighbors <- listDirectory basedir
               return $ (basedir </>) <$> neighbors) 
  else return []

findArt :: FilePath -> IO (Maybe FilePath)
findArt dir = do
  neighbors <- getNeighbors dir
  let cover = padCover $ filter isValidImgFormat neighbors
  return cover
    where padCover images | null images = Nothing | otherwise = Just (maximum images)
          strToLower = map toLower

isValidImgFormat :: FilePath -> Bool
isValidImgFormat file = or $ flipMap loFile $ map endswith validImgFormats
  where loFile = map toLower file
        flipMap = map . flip ($)
        validImgFormats = [".jpg",".jpeg",".png",".gif"]

-- findLargestArt' :: FilePath -> IO (Maybe Art)
-- findLargestArt' dir = do
--   neighbors <- getNeighbors dir
--   let candidates = filter isValidImgFormat neighbors
--   arts <- mapM findDimensions candidates
--   if null arts
--      then return Nothing
--      else return $ maximum arts

findLargestArt :: FilePath -> IO (Maybe Cover)
findLargestArt dir = do
  neighbors <- getNeighbors dir
  let candidates = filter isValidImgFormat neighbors
  loadedCandidates <- mapM loadCover candidates
  return $ if null loadedCandidates then Nothing else maximum loadedCandidates
