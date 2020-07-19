{-# LANGUAGE OverloadedStrings #-}

module Intcode.Reader (readText, ReadError (..)) where

import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Char (isSpace)
import           Data.Text (Text)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, ParseErrorBundle, eof, errorBundlePretty, label, sepBy, takeWhileP)
import qualified Text.Megaparsec as Parsec
import           Text.Megaparsec.Char (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

data ReadError = ReadError (ParseErrorBundle Text Void) String

readText :: Text -> Either ReadError (Vector Int)
readText = first toLoadError . Parsec.parse (sc *> integersP <* eof) ""
  where
    toLoadError e = ReadError e (errorBundlePretty e)

type Parser a = Parsec Void Text a

integersP :: Parser (Vector Int)
integersP = Vector.fromList <$> sepBy integerP comma

integerP :: Parser Int
integerP = label "integer" $ lexeme (signed sc decimal)

comma :: Parser ()
comma = void (char ',')

sc :: Parser ()
sc = void $ takeWhileP (Just "space") isSpace

lexeme :: Parser a -> Parser a
lexeme p = p <* sc
