{-# LANGUAGE OverloadedStrings #-}

module Intcode.Syntax
  ( Line (..)
  , InstructionOrDirective (..)
  , Arg (..)
  , Label (..)
  , Comment (..)
  , Mnemonic (..)
  , Directive (..)
  , Literal (..)
  , parse
  , ParseError (..)
  )
where

import           Control.Applicative ((<|>), optional, many)
import           Control.Monad (mzero, void)
import           Data.Bifunctor (first)
import           Data.Char (isAlphaNum)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import           Text.Megaparsec (
                    Parsec, ParseErrorBundle, eof, errorBundlePretty, label,
                    manyTill, takeWhileP, takeWhile1P, try)
import qualified Text.Megaparsec as Parsec
import           Text.Megaparsec.Char (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed, charLiteral)

data Line = Line (Maybe Label) (Maybe InstructionOrDirective) (Maybe Comment)
  deriving (Eq, Show)

data InstructionOrDirective
  = Instruction Mnemonic [Arg]
  | Directive Directive [Literal]
  deriving (Eq, Show)

data Arg
  = Number Int -- 1
  | ImmediateNumber Int -- #2
  | RelativeNumber Int -- 3,R
  | Label Label -- loopstart
  deriving (Eq, Show)

data Literal
  = I Int
  | T Text
  deriving (Eq, Show)

newtype Label = L Text
  deriving (Eq, Show)

newtype Comment = C Text
  deriving (Eq, Show)

newtype Mnemonic = M Text
  deriving (Eq, Show)

newtype Directive = D Text
  deriving (Eq, Show)

-- Parser ---

type Parser a = Parsec Void Text a

data ParseError = ParseError (ParseErrorBundle Text Void) String
  deriving (Eq, Show)

parse :: Text -> Either ParseError [Line]
parse = first toParseError . Parsec.parse (sc *> linesP <* eof) ""
  where
    toParseError e = ParseError e (errorBundlePretty e)

linesP :: Parser [Line]
linesP = many lineP-- `sepBy` eol

lineP :: Parser Line
lineP = lexeme $ do
  labelName <- optional (try labelP) -- labels overlap with opcodes (mnemonics)
  opcode <- optional opcodeP
  comment <- optional commentP
  end <- optional eol
  -- Blank lines must end in a newline. Otherwise, a newline is not required.
  if isNothing labelName && isNothing opcode && isNothing comment && isNothing end
    then mzero
    else return $ Line labelName opcode comment

labelP :: Parser Label
labelP = label "label" $ lexeme $ do
  name <- labelNameP
  void $ char ':'
  return name

labelNameP :: Parser Label
labelNameP = L <$> takeWhile1P Nothing isLabelChar
  where
    isLabelChar c = isAlphaNum c || c == '-' || c == '_'

opcodeP :: Parser InstructionOrDirective
opcodeP = instructionOpcodeP <|> directiveOpcodeP
  where
    instructionOpcodeP = Instruction <$> mnemonicP <*> many argP
    directiveOpcodeP = Directive <$> directiveP <*> many literalP

directiveP :: Parser Directive
directiveP = label "directive" $ lexeme $ do
  void $ char '.'
  name <- takeWhile1P Nothing isDirectiveChar
  return $ D name
  where
    isDirectiveChar c = isAlphaNum c || c == '-' || c == '_'

literalP :: Parser Literal
literalP = lexeme (intP <|> textP)
  where
    intP = I <$> signed sc decimal
    textP = T . Text.pack <$ quote <*> manyTill charLiteral quote
    quote = char '"'

mnemonicP :: Parser Mnemonic
mnemonicP = label "mnemonic" $ lexeme $ do
  name <- takeWhile1P Nothing isMnemonicChar
  return $ M name
  where
    isMnemonicChar c = isAlphaNum c || c == '-' || c == '_'

argP :: Parser Arg
argP = lexeme (immediateArgP <|> absoluteOrRelativeArgP)

immediateArgP :: Parser Arg
immediateArgP = label "immediate value" $ do
  void $ char '#'
  value <- numberP
  return $ ImmediateNumber value

absoluteOrRelativeArgP :: Parser Arg
absoluteOrRelativeArgP = absoluteOrRelativeNumberArgP <|> labelArgP
  where
    absoluteOrRelativeNumberArgP = possiblyRelative numberP Number RelativeNumber
    labelArgP = Label <$> labelNameP -- "label,R" not supported
    possiblyRelative parser mkAbsolute mkRelative = do
      value <- parser
      register <- optional (symbol "," *> symbol "R")
      return $
        case register of
          Nothing -> mkAbsolute value
          Just _  -> mkRelative value

numberP :: Parser Int
numberP = label "number" $ signed sc decimal

commentP :: Parser Comment
commentP = label "comment" $ lexeme $ do
  void $ symbol ";"
  text <- takeWhileP (Just "character") (/= '\n')
  return $ C text

-- | @sc@ is the horizontal white space consumer.
sc :: Parser ()
sc = void $ takeWhileP (Just "space") isHorizontalSpace
  where
    isHorizontalSpace c = c == ' ' || c == '\t'

-- | A lexeme is token that also consumes all trailing horizontal whitespace.
--
-- Use the @lexeme@ function to turn a token parser into a parser that consumes
-- tailing horizontal whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* sc

-- | A symbol is a constant string lexeme.
symbol :: Text -> Parser Text
symbol = lexeme . string

