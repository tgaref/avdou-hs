{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Document
  ( 
    Document(..)
  , splitFrontMatter
  , parseContext
  , load
  , markdownToHtml
  , docPathL
  , docContentL
  , docMetaL
  ) where

import           RIO
import qualified RIO.Text as T
import           RIO.Text.Partial (splitOn)
import qualified Data.Yaml as Yaml
import           Data.Aeson (Value(..))
import           Text.Pandoc
import           Context (Context(..))

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

load :: FilePath -> IO Document
load file = do
  content <- readFileUtf8 file
  let (header, rest) = splitFrontMatter content
  case header of
    Nothing  -> pure $ Document file rest mempty
    (Just h) -> let meta = parseContext h
      in pure $ Document file rest meta
  

-- Split front matter from Markdown
splitFrontMatter :: Text -> (Maybe Text, Text)
splitFrontMatter txt =
    case splitOn "---" txt of
      (_:header:rest) -> (Just (T.strip header), T.intercalate "---" rest)
      _               -> (Nothing, txt)

-- Parse header into Context map
parseContext :: Text -> Context
parseContext header =
    case Yaml.decodeEither' (encodeUtf8 header) of
      Left err    -> error (show err)
      Right (Object o) -> Context o -- o :: HashMap Text Value
      Right _          -> error "Expected a YAML mapping at top level"


-- Filters
markdownToHtml :: Document -> Document
markdownToHtml doc =
  let readerOpts = def
      writerOpts = def { writerHighlightStyle = Nothing } -- example customization
  in case runPure $ do
       pandoc <- readMarkdown readerOpts (view docContentL doc)
       writeHtml5String writerOpts pandoc
     of
       Left _   -> error $ "Failed to convert file " <> view docPathL doc
       Right html -> set docContentL html doc  
      
