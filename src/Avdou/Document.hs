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
  , copyfileCompiler
  ) where

import           RIO
import qualified RIO.Text as T
import           RIO.Text.Partial (splitOn)
import qualified Data.Yaml as Yaml
import           Data.Aeson (Value(..))
import           Text.Pandoc
import           Avdou.Shortcode (ShortcodeConfig, expandShortcodes)
import           Avdou.Types


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
markdownCompiler :: Document -> Document
markdownCompiler doc =
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
       pandoc <- readMarkdown readerOpts (view docContentL doc)
       writeHtml5String writerOpts pandoc
     of
       Left _   -> error $ "Failed to convert file " <> view docPathL doc
       Right html -> set docContentL html doc  

shortcodeCompiler :: ShortcodeConfig -> Document -> Document
shortcodeCompiler shortcodes doc =
  set docContentL (expandShortcodes shortcodes (view docContentL doc)) doc

copyfileCompiler :: Document -> Document
copyfileCompiler = id
