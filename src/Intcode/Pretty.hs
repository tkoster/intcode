{-# LANGUAGE OverloadedStrings #-}

module Intcode.Pretty (pretty, prettyBuilder) where

import           Data.Foldable (foldMap)
import           Data.Maybe (catMaybes, isJust)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)

import           Intcode.Syntax

pretty :: [Line] -> Text
pretty = toStrict . toLazyText . prettyBuilder

prettyBuilder :: [Line] -> Builder
prettyBuilder = foldMap buildLine

buildLine :: Line -> Builder
buildLine (Line label instructionOrDirective comment) =
  mconcat . catMaybes $
    [ buildLabel <$> label
    , buildInstructionOrDirective <$> instructionOrDirective
    , if (isJust label || isJust instructionOrDirective) && isJust comment then Just " " else Nothing
    , buildComment <$> comment
    , Just "\n"
    ]

buildInstructionOrDirective :: InstructionOrDirective -> Builder
buildInstructionOrDirective (Instruction mnemonic args) = "\t" <> buildMnemonic mnemonic <> buildArgs args
buildInstructionOrDirective (Directive directive args) = buildDirective directive <> buildLiteralArgs args

buildLabel :: Label -> Builder
buildLabel (L name) = fromText name <> ":"

buildMnemonic :: Mnemonic -> Builder
buildMnemonic (M name) = fromText name

buildArgs :: [Arg] -> Builder
buildArgs = foldMap $ \a -> " " <> buildArg a

buildArg :: Arg -> Builder
buildArg (Number value)           = decimal value
buildArg (ImmediateNumber value)  = "#" <> decimal value
buildArg (RelativeNumber value)   = decimal value <> ",R"
buildArg (Label (L name))         = fromText name

buildDirective :: Directive -> Builder
buildDirective (D name) = "." <> fromText name

buildLiteralArgs :: [Literal] -> Builder
buildLiteralArgs = foldMap $ \a -> " " <> buildLiteral a

buildLiteral :: Literal -> Builder
buildLiteral (I value) = decimal value
buildLiteral (T value) = fromString (show value)

buildComment :: Comment -> Builder
buildComment (C text) = "; " <> fromText text
