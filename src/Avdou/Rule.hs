{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Avdou.Rule
  ( executeRule
  , executeCopy
  ) where

import           RIO
import qualified RIO.Directory as Dir
import qualified RIO.Text as T
import qualified RIO.HashMap as HM (lookupDefault)
import           System.FilePath (takeDirectory, (</>), makeRelative)
import           Data.Aeson (ToJSON (toJSON))
import           Text.Mustache (substitute)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           GHC.Conc (numCapabilities)

import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

import           Avdou.Document
import           Avdou.Context
import           Avdou.Types
import Avdou.Pattern (expandPattern)
import RIO.Directory (copyFile)
import Avdou.Types (publicDirL, applyTemplate)
import Avdou.Document (Document)

-- Functions

myCopy :: MonadIO m => Site -> Route -> FilePath -> m ()
myCopy site route src = do
  let publicDir = view publicDirL site
      siteDir = view siteDirL site
      relSrc = makeRelative siteDir src
      dst = publicDir </> route relSrc
  Dir.createDirectoryIfMissing True (takeDirectory dst)
  Dir.copyFile src dst

executeCopy :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => Site -> Copy -> m ()
executeCopy site (Copy pat route) = do
  let siteDir = view siteDirL site
  files <- expandPattern siteDir pat
  let stream = S.fromList files
               & S.filterM Dir.doesFileExist
               & S.mapM (myCopy site route)
--               & S.parMapM cfg (myCopy site route)
  S.fold Fold.drain stream

--  where
--    cfg = S.maxThreads numCapabilities . S.ordered True

executeRule :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Site -> Rule -> m ()
executeRule site (Rule pat filters templates route splitMeta) = do
  let siteDir = view siteDirL site
      publicDir = view publicDirL site      
  files <- expandPattern siteDir pat
  let stream = S.fromList files
               & S.filterM Dir.doesFileExist
               & S.mapM (load splitMeta)
               & S.mapM (applyFilters filters)
               & fmap (applyTemplates site templates)
               & S.mapM (myWriteToFile siteDir publicDir route)
  S.fold Fold.drain stream

applyFilters :: MonadIO m => [Filter] -> Document -> m Document
applyFilters filters doc =
  S.fold (Fold.foldlM' (\acc f -> liftIO (f acc)) (pure doc)) $ S.fromList filters

applyTemplates :: Site -> [(Text, Context)] -> Document -> Document
applyTemplates site templates doc = set docContentL newContent doc
  where 
    ts = view templatesL site
    meta = view docMetaL doc 
    newContent = foldl' (\text (tmpl, ctx) ->
                            let newCtx' = mergeCtx meta ctx
                                newCtx  = insertCtx "content" (toJSON (text :: Text)) newCtx'
                            in substitute (HM.lookupDefault (error ("template " <> T.unpack tmpl <> " not found")) tmpl ts) newCtx
                        )
                 (view docContentL doc)
                 templates
                       
myWriteToFile :: (MonadIO m) => FilePath -> FilePath -> Route -> Document -> m ()  
myWriteToFile siteDir publicDir route doc = do
  let src = view docPathL doc
      relSrc = makeRelative siteDir src
      dst = publicDir </> route relSrc
  Dir.createDirectoryIfMissing True (takeDirectory dst)
  writeFileUtf8 dst (view docContentL doc)


{-
executeCopy :: MonadIO m => Site -> Copy -> m ()
executeCopy site (Copy pat route) = do
  let siteDir = view siteDirL site
  files' <- expandPattern siteDir pat
  files <- filterM Dir.doesFileExist files'
  forM_ files $ \src -> do
    let publicDir = view publicDirL site
        relSrc = makeRelative siteDir src
        dst = publicDir </> route relSrc
    Dir.createDirectoryIfMissing True (takeDirectory dst)
    Dir.copyFile src dst

executeRule :: MonadIO m => Site -> Rule -> m ()
executeRule site (Rule pat filters templates route splitMeta) = do
  let siteDir = view siteDirL site
  files' <- expandPattern siteDir pat
  files <- filterM Dir.doesFileExist files'
  forM_ files $ \src -> do
    -- load the document from disc
    doc <- load splitMeta src
      
    -- apply filters
    newDoc <- foldM (\acc f -> liftIO (f acc)) doc filters  
      
    -- apply templates
    let ts   = view templatesL site
        meta = view docMetaL newDoc 
        newContent = foldl' (\text (tmpl, ctx) ->
                               let newCtx' = mergeCtx meta ctx
                                   newCtx  = insertCtx "content" (toJSON (text :: Text)) newCtx'
                               in substitute (HM.lookupDefault (error ("template " <> T.unpack tmpl <> " not found")) tmpl ts) newCtx
                            )
                     (view docContentL newDoc)
                     templates
                       
        finalDoc = set docContentL newContent newDoc
                     
    -- save to disc
    let publicDir = view publicDirL site
        relSrc = makeRelative siteDir src
        dst = publicDir </> route relSrc
    Dir.createDirectoryIfMissing True (takeDirectory dst)
    writeFileUtf8 dst (view docContentL finalDoc)
-}
