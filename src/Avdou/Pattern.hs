{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Avdou.Pattern
  ( expandPattern
  ) where

import           RIO
import qualified RIO.Text as T
import           System.FilePath.Glob (glob)
import           Avdou.Types
import           RIO.HashSet (union, intersection, fromList, member)
import           System.FilePath ((</>))

expandPattern :: FilePath -> Pattern -> IO [FilePath]
expandPattern siteDir pat = do
  case pat of
    Simple p -> glob (siteDir </> T.unpack p)

    And p1 p2 -> do
      setp1 <- fromList <$> glob (siteDir </> T.unpack p1)
      setp2 <- fromList <$> glob (siteDir </> T.unpack p2)
      pure $ toList (setp1 `intersection` setp2)

    Or p1 p2 -> do 
      setp1 <- fromList <$> glob (siteDir </> T.unpack p1)
      setp2 <- fromList <$> glob (siteDir </> T.unpack p2)
      pure $ toList (setp1 `union` setp2)

    Diff p1 p2 -> do
      lp1 <- glob (siteDir </> T.unpack p1)
      setp2 <- fromList <$> glob (siteDir </> T.unpack p2)
      pure $ filter (\p -> not (p `member` setp2)) lp1
