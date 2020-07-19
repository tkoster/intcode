{-# LANGUAGE OverloadedStrings #-}

module Intcode.Writer (writeText) where

import           Data.Function ((&))
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector

writeText :: Vector Int -> Text
writeText program =
  program & Vector.toList
          & map Builder.decimal
          & intersperse ","
          & mconcat
          & (<> "\n")
          & Builder.toLazyText
          & Text.toStrict
