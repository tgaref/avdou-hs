{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Avdou.Document
  ( 
    Document(..)
  , splitFrontMatter
  , parseContext
  , load
  , markdownCompiler
  , shortcodeCompiler
  , copyFileCompiler
  ) where

import           RIO
import qualified RIO.Text as T
import           RIO.Text.Partial (splitOn)
import qualified Data.Yaml as Yaml
import           Data.Aeson (Value(..))
import           Text.Pandoc
import           Avdou.Shortcode (ShortcodeConfig, expandShortcodes)
import           Avdou.Types

load :: FilePath -> Bool -> IO Document
load fp splitMeta = do
  txt <- readFileUtf8 fp
  if splitMeta
    then do
    let (header, rest) = splitFrontMatter txt
    case header of
      Nothing  -> pure $ Document fp rest mempty
      (Just h) -> let meta = parseContext (encodeUtf8 h)
                  in pure $ Document fp rest meta
    else pure $ Document fp txt mempty
 

-- Split front matter from Markdown

splitFrontMatter :: Text -> (Maybe Text, Text)
splitFrontMatter txt =
    case splitOn "---" txt of
      (_:header:rest) -> (Just (T.strip header), T.intercalate "---" rest)
      _               -> (Nothing, txt)

-- Parse header into Context map
parseContext :: ByteString -> Context
parseContext header =
    case Yaml.decodeEither' header of
      Left err    -> error (show err)
      Right (Object o) -> Context o -- o :: HashMap Text Value
      Right _          -> error "Expected a YAML mapping at top level"

------------------------------------------------------------
-- Helper functions
------------------------------------------------------------
withTextContent
  :: MonadIO m
  => (Text -> m Text)
  -> Document
  -> m Document
withTextContent f doc = do
  newTxt <- f (view docContentL doc)
  pure $ set docContentL newTxt doc 

-- Filters
markdownToHtml :: Text -> IO Text
markdownToHtml txt =
  let readerOpts = def { readerExtensions = mathExts <> readerExtensions def }
      mathExts' = extensionsFromList
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_tex_math_single_backslash
        , Ext_latex_macros
        , Ext_fenced_divs
        , Ext_raw_html
--        , Ext_smart
        , Ext_markdown_in_html_blocks
        ]

--        raw_html+fenced_divs+markdown_in_html_blocks+smart
      mathExts = disableExtension Ext_raw_tex  mathExts'
      writerOpts = def { writerHTMLMathMethod = MathJax "" }

  in case runPure $ do
       pandoc <- readMarkdown readerOpts txt
       writeHtml5String writerOpts pandoc
     of
       Left _     -> error $ "Failed to convert file " <> T.unpack (T.take 100 txt)
       Right html -> pure html

markdownCompiler :: Document -> IO Document
markdownCompiler = withTextContent markdownToHtml

shortcodeExpander :: ShortcodeConfig -> Text -> IO Text
shortcodeExpander shortcodes txt =
  pure $ expandShortcodes shortcodes txt

shortcodeCompiler :: ShortcodeConfig -> Document -> IO Document
shortcodeCompiler shortcodes = withTextContent (shortcodeExpander shortcodes)

copyFileCompiler :: Document -> IO Document
copyFileCompiler = pure
