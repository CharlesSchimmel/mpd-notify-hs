{-# LANGUAGE OverloadedStrings #-}

module Options
  ( 
  BGStyle(..)
, Opti(..)
, getOptions
, libraryPath
, opti
, opts
  ) where

import Data.Monoid
import Options.Applicative

import PathHelpers

data Opti = Opti
    { libPath :: FilePath
    , port :: Int
    , host :: String
    , bgStyle :: BGStyle
    }

data BGStyle = Center | Tile | Fill | Max | Scale | None
  deriving ( Read, Show )

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
  <*> option auto
    ( long "bg-style"
    <> help "Fill type for displaying the album art as a background"
    <> showDefault
    <> value Center
    <> metavar "Center|Tile|None" )

opts :: ParserInfo Opti
opts = info (opti <**> helper) (fullDesc <> progDesc "Notifications for MPD" <> header "mpd-notify-hs - Notifcations, automatic wallpapers, and more for MPD")

getOptions :: IO Opti
getOptions = customExecParser (prefs $ columns 200) opts

libraryPath :: IO String
libraryPath = absolutize =<< libPath <$> getOptions
