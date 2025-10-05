{-# LANGUAGE NoImplicitPrelude #-}

module Avdou.Types
  ( Site(..)
  , siteDirL
  , publicDirL
  , templatesL
  , rulesL
  , copiesL
  , Filter
  , Rule(..)
  , ruleFiltersL
  , rulePatternL
  , ruleRouteL
  , ruleTemplatesL
  , Copy (..)
  , copyPatternL
  , copyRouteL
  , Document (..)
  , docContentL
  , docPathL
  , docMetaL
  , Pattern (..)
  , Context (..)
  , Route
  , HasSite (..)
  , Mine (..)
  , minePatternL
  , mineWorkersL
  ) where

import           RIO
import           Text.Mustache (ToMustache(..), Template)
import           Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM

-- Definition of Site
data Site = Site
  { _siteDir   :: !FilePath 
  , _publicDir :: !FilePath
  , _templates :: !(HashMap Text Template)
  , _copies    :: [Copy]
  , _rules     :: [Rule]
  }

siteDirL :: Lens' Site FilePath 
siteDirL = lens _siteDir (\site dir -> site {_siteDir = dir})

publicDirL :: Lens' Site FilePath 
publicDirL = lens _publicDir (\site dir -> site {_publicDir = dir})

templatesL :: Lens' Site (HashMap Text Template)
templatesL = lens _templates (\site hm -> site {_templates = hm}) 

rulesL :: Lens' Site [Rule]
rulesL = lens _rules (\site rules -> site {_rules = rules})

copiesL :: Lens' Site [Copy]
copiesL = lens _copies (\site copies -> site {_copies = copies})

-- Definition of Filter
type Filter = Document -> Document

-- Definition of Rule and Copy 
data Rule = Rule
  { _rulePattern   :: !Pattern
  , _ruleFilters   :: [Filter]
  , _ruleTemplates :: [(Text, Context)]
  , _ruleRoute     :: Route 
  }

data Copy = Copy
  { _copyPattern :: !Pattern
  , _copyRoute   :: Route
  }

rulePatternL :: Lens' Rule Pattern
rulePatternL = lens _rulePattern (\rule pat -> rule {_rulePattern = pat})

ruleFiltersL :: Lens' Rule [Filter]
ruleFiltersL = lens _ruleFilters (\rule filters -> rule {_ruleFilters = filters})

ruleTemplatesL :: Lens' Rule [(Text, Context)]
ruleTemplatesL = lens _ruleTemplates (\rule ts -> rule {_ruleTemplates = ts})

ruleRouteL :: Lens' Rule Route
ruleRouteL = lens _ruleRoute (\rule route -> rule {_ruleRoute = route})

copyPatternL :: Lens' Copy Pattern
copyPatternL = lens _copyPattern (\copy pat -> copy {_copyPattern = pat})

copyRouteL :: Lens' Copy Route
copyRouteL = lens _copyRoute (\copy route -> copy {_copyRoute = route})

data Document = Document
  { _docPath     :: !FilePath
  , _docContent  :: !Text
  , _docMeta :: !Context
  }

docPathL :: Lens' Document FilePath
docPathL = lens _docPath (\doc path -> doc {_docPath = path})

docContentL :: Lens' Document Text
docContentL = lens _docContent (\doc content -> doc {_docContent = content})

docMetaL :: Lens' Document Context
docMetaL = lens _docMeta (\doc meta -> doc {_docMeta = meta})

data Pattern = Simple !Text
             | And !Text !Text
             | Or  !Text !Text
             | Diff !Text !Text

type Route = FilePath -> FilePath 

newtype Context = Context {unContext :: Aeson.Object}
  deriving (Eq, Show)

instance Semigroup Context where
  Context o1 <> Context o2 = Context $ KM.union o1 o2 

instance Monoid Context where
  mempty = Context mempty

instance ToMustache Context where
  toMustache (Context obj) = toMustache (Aeson.Object obj)

class HasSite env where
  siteL :: Lens' env Site

instance HasSite Site where
  siteL = id
  
data Mine = Mine {
    _minePattern :: !Pattern
  , _mineWorkers :: [Document -> Context]
  }

minePatternL :: Lens' Mine Pattern
minePatternL = lens _minePattern (\mine pat -> mine {_minePattern = pat})

mineWorkersL :: Lens' Mine [Document -> Context]
mineWorkersL = lens _mineWorkers (\mine ws -> mine {_mineWorkers = ws})
