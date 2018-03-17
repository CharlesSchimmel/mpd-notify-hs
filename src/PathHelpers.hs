module PathHelpers
  ( 
  absolutize
, getNeighbors
, findArt
, findLargestArt
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.FilePath
import System.Path.NameManip
import Data.Char
import Data.Maybe
import Data.List
import Data.String.Utils

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
-- could possible also `try` this with fail condition `return []`
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
  let cover = padCover $ filter (isValidImgFormat) neighbors
  return cover
    where padCover images | null images = Nothing | otherwise = Just (maximum $ images)
          strToLower = map toLower

findLargestArt :: FilePath -> IO (Maybe Art)
findLargestArt dir = do
  neighbors <- getNeighbors dir
  let candidates = filter (isValidImgFormat) neighbors
  arts <- mapM findDimensions candidates
  if null arts
     then return Nothing
     else return $ maximum $ arts

isValidImgFormat :: FilePath -> Bool
isValidImgFormat file = foldr (||) False $ flipMap loFile $ map endswith validImgFormats
  where loFile = map toLower file
        flipMap = map . flip ($)
        validImgFormats = [".jpeg",".png",".gif"]
