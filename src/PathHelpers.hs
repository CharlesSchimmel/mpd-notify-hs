module PathHelpers
  ( 
  absolutize
, getNeighbors
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.FilePath
import System.Path.NameManip
import Data.Maybe
import Data.List

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
getNeighbors file = do
  doesExist <- doesPathExist =<< absolutize file
  if doesExist then (do
               basedir <- takeDirectory <$> absolutize file
               neighbors <- listDirectory basedir
               return $ (basedir </>) <$> neighbors) 
  else return []
