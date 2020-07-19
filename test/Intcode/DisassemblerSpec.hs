{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Intcode.DisassemblerSpec (spec) where

import           Test.Hspec

import           Intcode.Disassembler
import           Intcode.Syntax

spec :: Spec
spec = do
  describe "disassemble" $ do

    it "disassembles a nullary instruction" $
      disassemble [99] `shouldBe` [Line Nothing (Just $ Instruction (M "hlt") []) Nothing]

    it "disassembles an immediate argument" $
      disassemble [109, 1] `shouldBe` [Line Nothing (Just $ Instruction (M "rel") [ImmediateNumber 1]) Nothing]

    it "disassembles a relative argument" $
      disassemble [204, 1] `shouldBe` [Line Nothing (Just $ Instruction (M "out") [RelativeNumber 1]) Nothing]

    it "disassembles unknown nonnegative values as data" $
      disassemble [1234] `shouldBe` [Line Nothing (Just $ Directive (D "data") [I 1234]) Nothing]

    it "disassembles negative values as data" $
      disassemble [-1] `shouldBe` [Line Nothing (Just $ Directive (D "data") [I (-1)]) Nothing]

    it "disassembles 3,9,8,9,10,9,4,9,99,-1,8" $ do
      let input = [3,9,8,9,10,9,4,9,99,-1,8]
          ast =
            [ Line Nothing (Just $ Instruction (M "in") [Number 9]) Nothing
            , Line Nothing (Just $ Instruction (M "teq") [Number 9, Number 10, Number 9]) Nothing
            , Line Nothing (Just $ Instruction (M "out") [Number 9]) Nothing
            , Line Nothing (Just $ Instruction (M "hlt") []) Nothing
            , Line Nothing (Just $ Directive (D "data") [I (-1)]) Nothing
            , Line Nothing (Just $ Directive (D "data") [I 8]) Nothing
            ]
      disassemble input `shouldBe` ast

    it "disassembles the quine program" $ do
      let quineAST =
            [ Line Nothing (Just $ Instruction (M "rel") [ImmediateNumber 1]) Nothing
            , Line Nothing (Just $ Instruction (M "out") [RelativeNumber (-1)]) Nothing
            , Line Nothing (Just $ Instruction (M "add") [Number 100, ImmediateNumber 1, Number 100]) Nothing
            , Line Nothing (Just $ Instruction (M "teq") [Number 100, ImmediateNumber 16, Number 101]) Nothing
            , Line Nothing (Just $ Instruction (M "bf")  [Number 101, ImmediateNumber 0]) Nothing
            , Line Nothing (Just $ Instruction (M "hlt") []) Nothing
            ]
          quineProg = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
      disassemble quineProg `shouldBe` quineAST

