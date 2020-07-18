{-# LANGUAGE OverloadedStrings #-}

module Intcode.PrettySpec (spec) where

import qualified Data.Text.IO as Text
import           Test.Hspec

import           Intcode.Pretty (pretty)
import           Intcode.Syntax

spec :: Spec
spec =
  describe "pretty" $ do

    it "renders a blank line" $
      pretty [Line Nothing Nothing Nothing] `shouldBe` "\n"

    it "renders a line containing only a label" $
      pretty [Line (Just $ L "label") Nothing Nothing] `shouldBe` "label:\n"

    it "renders a line containing a label and a comment" $
      pretty [Line (Just $ L "label") Nothing (Just $ C "comment")] `shouldBe` "label: ; comment\n"

    it "renders an instruction containing only a mnemonic" $
      pretty [Line Nothing (Just $ Instruction (M "hlt") []) Nothing] `shouldBe` "\thlt\n"

    it "renders an instruction containing a label and a mnemonic" $
      pretty [Line (Just $ L "label") (Just $ Instruction (M "hlt") []) Nothing] `shouldBe` "label:\thlt\n"

    it "renders an instruction with the lot" $
      pretty [Line (Just $ L "label") (Just $ Instruction (M "add") [RelativeNumber 1, ImmediateNumber 2, Label (L "three")]) (Just $ C "comment")]
        `shouldBe` "label:\tadd 1,R #2 three ; comment\n"

    it "renders the quine program (parse-pretty round trip)" $ do
      quineSource <- Text.readFile "test/quine.s"
      let Right quineAST = parse quineSource
      pretty quineAST `shouldBe` quineSource
