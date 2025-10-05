{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Avdou.Shortcode
  ( ShortcodeConfig
  , expandShortcodes
  ) where

import RIO hiding (try, many, some)
import qualified RIO.Text as T
import qualified RIO.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Char as Char


type Parser = Parsec Void Text
type ShortcodeConfig = Map Text ([Text] -> Text)


expandShortcodes :: Map Text ([Text] -> Text) -> Text -> Text
expandShortcodes table input =
  case runParser (process table) "<input>" input of
    Left _  -> input
    Right t -> t

process :: Map Text ([Text] -> Text) -> Parser Text
process table = T.concat <$> many (choice
  [ try (shortcode table)   -- must come first!
  , textChunk
  , backslashLiteral        -- optional
  ])
  
textChunk :: Parser Text
textChunk = takeWhile1P Nothing (/= '\\')

shortcode :: Map Text ([Text] -> Text) -> Parser Text
shortcode table = try $ do
  void $ char '\\'
  name <- takeWhile1P (Just "shortcode name") (\c -> Char.isAlphaNum c || c == '_')
  next <- lookAhead (optional anySingle)
  case next of
    Just '{' -> do
      args <- some braced
      pure $ case Map.lookup name table of
        Just f  -> f args
        Nothing -> "\\" <> name <> mconcat (wrap <$> args)
    _ -> pure ("\\" <> name)
  where
    wrap a = "{" <> a <> "}"

backslashLiteral :: Parser Text
backslashLiteral = do
  void $ char '\\'
  c <- anySingle
  pure $ "\\" <> T.singleton c

-- Handles nested {}, escaped \{ and \}, and multiline content
braced :: Parser Text
braced = do
  void $ char '{'
  inner <- T.concat <$> manyTill chunk (lookAhead $ char '}')
  void $ char '}'
  pure inner
  where
    chunk :: Parser Text
    chunk = nested <|> literal
    nested = do
      inner <- braced
      pure ("{" <> inner <> "}")
    literal = takeWhile1P Nothing (\c -> c /= '{' && c /= '}')
