{-# LANGUAGE NoImplicitPrelude #-}

module Route
  (Route
  , idRoute
  , setExtension
  ) where

import RIO
import RIO.FilePath (replaceExtension)

type Route = FilePath -> FilePath 

idRoute :: FilePath -> FilePath
idRoute = id

setExtension :: String -> FilePath -> FilePath
setExtension = flip replaceExtension
