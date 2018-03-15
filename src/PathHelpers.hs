module PathHelpers
  ( 
  absolutize
, getNeighbors
, findArt
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.FilePath
import System.Path.NameManip
import Data.Maybe
import Data.List
import Data.String.Utils

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
getNeighbors :: String -> IO [FilePath]
getNeighbors file = do
  doesExist <- doesPathExist =<< absolutize file
  if doesExist then (do
               basedir <- takeDirectory <$> absolutize file
               neighbors <- listDirectory basedir
               return $ (basedir </>) <$> neighbors) 
  else return []

findArt :: FilePath -> IO (Maybe [Char])
findArt dir = do
  neighbors <- getNeighbors dir
  let cover = padCover $ filter (endswith ".jpg") neighbors
  return cover
    where padCover images | null images = Nothing | otherwise = Just (maximum $ images)
