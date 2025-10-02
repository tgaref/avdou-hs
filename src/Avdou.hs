{-# LANGUAGE NoImplicitPrelude #-}
module Avdou
  ( Site(..)
  , siteDirL
  , publicDirL
  , templatesL
  ) where

import RIO
import Text.Mustache (Template)

data Site = Site
  { _siteDir   :: !FilePath 
  , _publicDir :: !FilePath
  , _templates :: !(HashMap Text Template)
  }

siteDirL :: Lens' Site FilePath 
siteDirL = lens _siteDir (\site dir -> site {_siteDir = dir})

publicDirL :: Lens' Site FilePath 
publicDirL = lens _publicDir (\site dir -> site {_publicDir = dir})

templatesL :: Lens' Site (HashMap Text Template)
templatesL = lens _templates (\site hm -> site {_templates = hm}) 
