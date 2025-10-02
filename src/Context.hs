{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Context
  (
    Context(..)
  , loadTemplates
  , insertCtx
  , mergeCtx
  , lookupCtx
  ) where

import           RIO
import           Text.Mustache (ToMustache(..), Template, automaticCompile)
import           RIO.Directory (listDirectory)
import           RIO.HashMap (fromList)
import qualified RIO.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

newtype Context = Context {unContext :: Aeson.Object}
  deriving (Eq, Show)

instance Semigroup Context where
  Context o1 <> Context o2 = Context $ KM.union o1 o2 

instance Monoid Context where
  mempty = Context mempty

instance ToMustache Context where
  toMustache (Context obj) = toMustache (Aeson.Object obj)


-- Insert a key/value
insertCtx :: Text -> Aeson.Value -> Context -> Context
insertCtx k v (Context obj) =
  Context (KM.insert (Key.fromText k) v obj)

-- Merge two Contexts (right-biased)
mergeCtx :: Context -> Context -> Context
mergeCtx (Context a) (Context b) = Context (KM.union b a)

-- Lookup a key
lookupCtx :: Text -> Context -> Maybe Aeson.Value
lookupCtx k (Context obj) = KM.lookup (Key.fromText k) obj

loadTemplates :: FilePath -> IO (HashMap Text Template)
loadTemplates path = do
  files <- listDirectory path
  eitherTemplates <- mapM (\file -> fmap (fmap (T.pack file, )) (automaticCompile [path] file)) files
  let templates = fmap (\case
                           Right t -> t
                           Left _  -> error "Failed to parse template"
                       ) eitherTemplates
  pure $ fromList templates

