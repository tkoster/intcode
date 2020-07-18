{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Intcode.AssemblerSpec (spec) where

import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Test.Hspec hiding (Arg)

import           Intcode.Assembler
import           Intcode.Syntax

spec :: Spec
spec = describe "assemble" $ do

  it "assembles data" $ do
    let program = parseThrow ".data 1 2 3 \"hello\" 4 5"
    assemble program `shouldBe` Right [1,2,3,104,101,108,108,111,4,5]

  it "reports an error when a label is undefined" $ do
    let program = parseThrow "bt 0 nonexistentlabel"
    assemble program `shouldBe` Left [UndefinedLabel "nonexistentlabel"]

  it "reports an error when a label is defined twice" $ do
    let program = parseThrow "duplicatelabel:\nduplicatelabel:"
    assemble program `shouldBe` Left [DuplicateLabel "duplicatelabel"]

  it "reports an error for an invalid mnemonic" $ do
    let program = parseThrow "foo"
    assemble program `shouldBe` Left [InvalidMnemonic "foo"]

  it "reports an error for the incorrect number of arguments" $ do
    let program = parseThrow "hlt 1"
    assemble program `shouldBe` Left [InvalidArgs "hlt" []]

  it "reports an error for an invalid argument mode" $ do
    let program = parseThrow "in #1"
    assemble program `shouldBe` Left [InvalidArgMode]

  it "reports an error for an invalid directive" $ do
    let program = parseThrow ".foo"
    assemble program `shouldBe` Left [InvalidDirective "foo"]

  it "reports an error for the incorrect number of directive arguments" $ do
    let program = parseThrow ".org 1 2 3"
    assemble program `shouldBe` Left [InvalidDirectiveArgs "org" ["address"]]

  it "reports an error for an incorrect type of directive argument" $ do
    let program = parseThrow ".org \"string\""
    assemble program `shouldBe` Left [InvalidDirectiveArgs "org" ["address"]]

  it "assembles the quine program" $ do
    quineSource <- Text.readFile "test/quine.s"
    let Right quineAST = parse quineSource
    assemble quineAST `shouldBe` Right [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

parseThrow :: Text -> [Line]
parseThrow input =
  case parse input of
    Right program -> program
    Left err      -> error (show err)
