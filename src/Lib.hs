#!/usr/bin/env runhaskell

module Lib
  ( mainLoop
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
import PathHelpers
import Safe as Sf
import System.Directory
import System.Environment
import System.FilePath
import System.Path.NameManip
import System.Process

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
  where unwrap = either (\_ -> Nothing) id

-- Maybe not best practice but MPD will (should) always return something for the filepath
getDir::Either a (Maybe Song) -> FilePath
getDir = either (\_ -> "") (maybe "" (\z -> toString $ sgFilePath z))

-- eventually we'll find the largest of the images...
getWidth file = do
  readProcess "identify" (["-format","%W",file]) ""

changeWall::String->FilePath->IO String
changeWall "tile" image = readProcess "feh" ["--bg-tile",image] ""
changeWall "center" image = readProcess "feh" ["--bg-center",image] ""
changeWall "stretch" image = readProcess "feh" ["--bg-maimage",image] ""
changeWall "smart" image = readProcess "feh" ["--bg-tile",image] ""
changeWall _ image = changeWall "tile" image

songNotif::Maybe Notification -> IO Notification
songNotif token = do
  cSong <- ($ currentSong) =<< mpdCon'
  let [artist,title,album] = (songAttr cSong) <$> [Artist,Title,Album]
      notifTemplate = summary (title ++ " - " ++ artist) <> body album
  cover <- findArt =<< (</> (getDir cSong)) <$> libraryPath
  let notif = maybe (notifTemplate) (\x -> notifTemplate <> icon x) cover
  updateArt cover
  maybe (display notif) (\x -> display $ reuse x <> notif) token

updateArt::Maybe FilePath -> IO String
updateArt cover = do
  maybe (return ("No cover found") :: IO String) (changeWall "tile") cover

statusNotif::Show a=> a -> Maybe Notification -> IO Notification
statusNotif state token = maybe (display (summary $ show $ state)) (\x -> display (reuse x <> (summary $ show $ state) <> body "")) token

mpdCon host port = withMPD_ (Just host) (Just $ show port)
mpdCon' = mpdBuilder <$> execParser opts 
  where mpdBuilder (Opti _ port host) = withMPD_ (Just host) (Just $ show port)

libraryPath::IO String
libraryPath = absolutize =<< libPath <$> execParser opts

waitForChange::MonadMPD f=>(f Status -> IO (Either a Status)) -> Maybe Notification -> IO Notification
waitForChange mpdConnection notif = do
  new <- curStat <$> ( mpdConnection $ idle [PlayerS] *> status )
  if (new /= Playing)
     then statusNotif new notif
     else songNotif notif
    where curStat = either (\x -> Stopped) stState

subLoop::Maybe Notification -> IO ()
subLoop notif = do
  args <- execParser opts
  let mpdConnection = mpdCon (host args) (port args)
  mpdStatus <- mpdConnection $ status
  if (isRight mpdStatus)
     then waitForChange mpdConnection notif >>= (\x -> subLoop (Just x))
     else putStrLn "Cannot connect to MPD. Check your host and port and make sure MPD is running."

mainLoop = do
  libraryValid <- doesPathExist =<< libraryPath
  when (not libraryValid) (putStrLn "Library path not found. Album art won't work.")
  subLoop Nothing
