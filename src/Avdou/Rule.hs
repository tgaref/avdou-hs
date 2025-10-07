{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Avdou.Rule
  ( executeRule
  , executeCopy
  ) where

import           Prelude (print)
import           RIO
import qualified RIO.Directory as Dir
import qualified RIO.Text as T
import           System.FilePath (takeDirectory, (</>), makeRelative)
import           Data.Aeson (ToJSON (toJSON))
import           Text.Mustache (substitute)
import qualified RIO.HashMap as HM (lookupDefault)
import           Avdou.Document
import           Avdou.Context
import           Avdou.Types
import Avdou.Pattern (expandPattern)

-- Functions

executeCopy :: Site -> Copy -> IO ()
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

executeRule :: Site -> Rule -> IO ()
executeRule site (Rule pat filters templates route) = do
  let siteDir = view siteDirL site
  files' <- expandPattern siteDir pat
  files <- filterM Dir.doesFileExist files'
  forM_ files $ \src -> do
    -- load the document from disc
    doc <- load src
    
    -- apply filters
    let newDoc = foldl' (\acc f -> f acc) doc filters  
      
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
