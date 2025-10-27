{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Avdou.Context
  ( loadTemplates
  , insertCtx
  , insertManyCtx
  , mergeCtx
  , lookupCtx
  , executeMine
  , mine
  ) where

import           RIO
import           Text.Mustache (Template, automaticCompile)
import           RIO.Directory (listDirectory)
import           RIO.HashMap (fromList)
import qualified RIO.Text as T
import           RIO.State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import           Avdou.Types
import           Avdou.Document (load)
import           Avdou.Pattern

import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold


-- Insert a key/value
insertCtx :: Text -> Aeson.Value -> Context -> Context
insertCtx k v (Context obj) =
  Context (KM.insert (Key.fromText k) v obj)

insertManyCtx :: Foldable t => t (Text, Aeson.Value) -> Context -> Context
insertManyCtx kvs (Context obj) =
  Context $ foldl' (\o (k,v) -> KM.insert (Key.fromText k) v o) obj kvs

-- Merge two Contexts (right-biased)
mergeCtx :: Context -> Context -> Context
mergeCtx (Context a) (Context b) = Context (KM.union b a)

-- Lookup a key
lookupCtx :: Text -> Context -> Maybe Aeson.Value
lookupCtx k (Context obj) = KM.lookup (Key.fromText k) obj

loadTemplates :: MonadIO m => FilePath -> m (HashMap Text Template)
loadTemplates path = do
  files <- listDirectory path
  eitherTemplates <- mapM (\file -> fmap (fmap (T.pack file, )) (liftIO $ automaticCompile [path] file)) files
  let templates = fmap (\case
                           Right t -> t
                           Left _  -> error "Failed to parse template"
                       ) eitherTemplates
  pure $ fromList templates

{-
executeMine' :: MonadIO m => FilePath -> Mine -> m (HashMap Text Context)
executeMine' siteDir (Mine pat workers splitMeta) = do
  files <- expandPattern siteDir pat    
  list <- forM files $ \file -> do
    ctx <- mineLocal file splitMeta workers 
    pure (T.pack file, ctx)

  pure $ fromList list
-}

executeMine :: MonadIO m => FilePath -> Mine -> m (HashMap Text Context)
executeMine siteDir (Mine pat workers splitMeta) = do
  files <- expandPattern siteDir pat
  stream <- S.fromList files
            & S.mapM (\file -> do
                         ctx <- mineLocal file splitMeta workers 
                         pure (T.pack file, ctx)
                     )
            & S.toList
  pure $ fromList stream
                          

mineLocal :: MonadIO m =>  FilePath -> Bool -> [Document -> Context] -> m Context
mineLocal file splitMeta workers = do
    doc <- load splitMeta file
    pure $ foldl' (\acc f -> mergeCtx (f doc) acc) mempty workers

mine :: (MonadIO m) => FilePath -> Pattern -> MineM m () -> m (HashMap Text Context)
mine siteDir pat builder = do
  let base = Mine pat [] True  -- By default, slit and parse Metadata
  mn <- execStateT (unMineM builder) base
  executeMine siteDir mn  
