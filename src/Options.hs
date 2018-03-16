module Options
  ( 
  opti
, opts
, Opti(..)
  ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Options.Applicative

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

opts :: ParserInfo Opti
opts = info (opti <**> helper) (fullDesc <> progDesc "Display notifications for MPD" <> header "mpd-notify-hs - Notifcations, automatic wallpapers, and more for MPD")

