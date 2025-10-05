{-# LANGUAGE NoImplicitPrelude #-}

module Avdou.Route
  ( idRoute
  , setExtension
  , niceRoute
  ) where

import RIO
import RIO.FilePath

import Avdou.Types

idRoute :: FilePath -> FilePath
idRoute = id

setExtension :: String -> FilePath -> FilePath
setExtension = flip replaceExtension

niceRoute :: FilePath -> FilePath
niceRoute fp = takeDirectory fp </> takeBaseName fp </> "index.html"
