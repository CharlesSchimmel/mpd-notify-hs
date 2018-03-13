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
import Data.List
import Data.Maybe
import Data.String.Utils
import Debug.Trace
import Libnotify
import Network.MPD
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath
import System.Path.NameManip
import System.Process
import Safe as Sf
import PathHelpers

data Opti = Opti
    { libPath :: FilePath
    , port :: Int
    , host :: String }

opti :: Parser Opti
opti = Opti
  <$> strOption 
    ( long "library"
    <> metavar "PATH"
    <> help "Path to music library" )
  <*> option auto
    ( long "port"
    <> help "Port of target MPD server"
    <> showDefault
    <> value 6600
    <> metavar "INT" )
  <*> option auto
    ( long "host"
    <> help "Host of target MPD server"
    <> showDefault
    <> value "localhost"
    <> metavar "STRING" )
  -- <*> option auto
  --   ( long "Background Style"
  --   <> help "Host of target MPD server"
  --   <> showDefault
  --   <> value "localhost"
  --   <> metavar "STRING" )

opts::ParserInfo Opti
opts = info (opti <**> helper) (fullDesc <> progDesc "Display notifications for MPD" <> header "mpd-notify-hs - Notifcations, automatic wallpapers, and more for MPD")

-- check tags for desired metadata. if exists, convert to String
songAttr::Either a (Maybe Song) -> Metadata -> String
songAttr song meta = maybe "<unkown>" (toString . head) (maybe Nothing (sgGetTag meta) (unwrap song))
  where unwrap = either (\x -> Nothing) id

-- Maybe not best practice but MPD will (should) always return something for the filepath
getDir::Either a (Maybe Song) -> FilePath
getDir = either (\x -> "") (maybe "" (\z -> toString $ sgFilePath z))

-- eventually we'll find the largest of the images...
getWidth file = do
  readProcess "identify" (["-format","%W",file]) ""

notifArt title artist album cover = display (summary (artist ++ " - " ++ title) <> body album <> icon cover)
notifPlain title artist album = display (summary (artist ++ " - " ++ title) <> body album)

changeWall::String->FilePath->IO String
changeWall "tile" image = readProcess "feh" ["--bg-tile",image] ""
changeWall "center" image = readProcess "feh" ["--bg-center",image] ""
changeWall "stretch" image = readProcess "feh" ["--bg-maimage",image] ""
changeWall "smart" image = readProcess "feh" ["--bg-tile",image] ""
changeWall _ image = changeWall "tile" image

songNotif = do
  cSong <- ($ currentSong) =<< mpdCon'
  let [curArtist,curTitle,curAlbum] = (songAttr cSong) <$> [Artist,Title,Album]
  neighbors <- getNeighbors =<< (</>(getDir cSong)) <$> libraryPath
  let cover = padCover $ filter (endswith ".jpg") neighbors
  maybe (return ("No cover found") :: IO String) (changeWall "tile") cover
  maybe (notifPlain curTitle curArtist curAlbum) (notifArt curTitle curArtist curAlbum) cover
    where padCover images | null images = Nothing | otherwise = Just (maximum $ images)

statusNotif::Show a=> a -> IO Notification
statusNotif state = display (summary $ show $ state)
mpdCon host port = withMPD_ (Just host) (Just $ show port)
mpdCon' = mpdBuilder <$> execParser opts 
  where mpdBuilder (Opti _ port host) = withMPD_ (Just host) (Just $ show port)

libraryPath::IO String
libraryPath = absolutize =<< libPath <$> execParser opts

-- waitForChange::MonadMPD m=>(m a => IO (Response b))->IO Notification
waitForChange mpdConnection = do
  old <- curStat <$> ( mpdConnection $ status )
  new <- curStat <$> ( mpdConnection $ idle [PlayerS] *> status )
  if (old == new) 
     then songNotif
     else statusNotif new

curStat = either (\x -> Stopped) (stState)

mainLoop = do
  libraryValid <- doesPathExist =<< libraryPath
  when (not libraryValid) (putStrLn "Library path not found. Album art won't work.")
  args <- execParser opts
  let mpdConnection = mpdCon (host args) (port args)
  mpdStatus <- mpdConnection $ status
  if (isRight mpdStatus)
     then waitForChange (mpdCon (host args) (port args)) >> mainLoop
     else putStrLn "Cannot connect to MPD. Check your host and port and make sure MPD is running."

foo mpdCon = do
  bar <- mpdCon currentSong
  -- (baz,bar) <- mpdCon <$> (status,currentSong)
  -- putStrLn baz
  putStrLn bar
