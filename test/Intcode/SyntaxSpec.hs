{-# LANGUAGE OverloadedStrings #-}

module Intcode.SyntaxSpec (spec) where

import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Test.Hspec

import           Intcode.Syntax

spec :: Spec
spec = do
  describe "parse" $ do

    specify "an empty string is zero lines" $
      "" `shouldParseInto` []

    specify "blank lines are accepted/preserved" $
      "\n" `shouldParseInto` [blank]

    specify "a line consisting only of spaces is a blank line" $ do
      " \n" `shouldParseInto` [blank]
      "\t\n" `shouldParseInto` [blank]

    specify "a line starting with a semicolon is a comment line" $
      ";comments here" `shouldParseInto` [Line Nothing Nothing (Just $ C "comments here")]

    specify "space before a comment is ignored" $ do
      " ;comment" `shouldParseInto` [Line Nothing Nothing (Just $ C "comment")]
      "\t;comment" `shouldParseInto` [Line Nothing Nothing (Just $ C "comment")]

    specify "a word ending in a colon is a label" $
      "label:" `shouldParseInto` [Line (Just $ L "label") Nothing Nothing]

    specify "space before a label is ignored" $
      " label:" `shouldParseInto` [Line (Just $ L "label") Nothing Nothing]

    specify "a commented label is accepted (1)" $
      "label:;comment" `shouldParseInto` [Line (Just $ L "label") Nothing (Just $ C "comment")]

    specify "a commented label is accepted (2)" $
      "label: ;comment" `shouldParseInto` [Line (Just $ L "label") Nothing (Just $ C "comment")]

    specify "a labelled opcode is accepted (1)" $
      "label:hlt" `shouldParseInto` [Line (Just $ L "label") (Just $ Instruction (M "hlt") []) Nothing]

    specify "a labelled opcode is accepted (2)" $
      "label: hlt" `shouldParseInto` [Line (Just $ L "label") (Just $ Instruction (M "hlt") []) Nothing]

    specify "a word not followed by a colon is a mnemonic" $
      "hlt" `shouldParseInto` [Line Nothing (Just $ Instruction (M "hlt") []) Nothing]

    specify "space before a mnemonic is ignored" $ do
      " hlt" `shouldParseInto` [Line Nothing (Just $ Instruction (M "hlt") []) Nothing]
      "\thlt" `shouldParseInto` [Line Nothing (Just $ Instruction (M "hlt") []) Nothing]

    specify "a mnemonic followed by a newline is accepted" $
      "hlt\n" `shouldParseInto` [Line Nothing (Just $ Instruction (M "hlt") []) Nothing]

    specify "a directive with two args is accepted" $
      ".foo 1 2 3" `shouldParseInto` [Line Nothing (Just $ Directive (D "foo") [I 1, I 2, I 3]) Nothing]

    specify "it accepts all the things" $
      "start:\tadd 1,R #2 three ;add some things" `shouldParseInto`
        [Line (Just $ L "start") (Just $ Instruction (M "add") [RelativeNumber 1, ImmediateNumber 2, Label (L "three")]) (Just $ C "add some things")]

    specify "it correctly parses the quine program" $ do
      quineSource <- Text.readFile "test/quine.s"
      let quineAST =
            [ Line Nothing Nothing (Just $ C "vim: tabstop=8 shiftwidth=8 textwidth=80 noexpandtab syntax=intcode")
            , blank
            , Line Nothing            (Just $ Directive (D "org") [I 0]) Nothing
            , blank
            , Line (Just $ L "start") (Just $ Instruction (M "rel") [ImmediateNumber 1]) Nothing
            , Line Nothing            (Just $ Instruction (M "out") [RelativeNumber (-1)]) Nothing
            , Line Nothing            (Just $ Instruction (M "add") [Number 100, ImmediateNumber 1, Number 100]) Nothing
            , Line Nothing            (Just $ Instruction (M "teq") [Number 100, ImmediateNumber 16, Number 101]) Nothing
            , Line Nothing            (Just $ Instruction (M "bf")  [Number 101, Label (L "start")]) Nothing
            , Line (Just $ L "end")   (Just $ Instruction (M "hlt") []) Nothing
            ]
      quineSource `shouldParseInto` quineAST

  where
    blank = Line Nothing Nothing Nothing

shouldParseInto :: Text -> [Line] -> Expectation
shouldParseInto input expected =
  case parse input of
    Right output -> output `shouldBe` expected
    Left (ParseError _ msg) -> fail msg
