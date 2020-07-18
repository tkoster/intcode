{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Intcode.Disassembler (disassemble, zipModeFlags) where

import           Data.Text (Text)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector

import           Intcode.Syntax

disassemble :: Vector Int -> [Line]
disassemble = go . Vector.toList
  where
    go [] = []
    go (opcode : args)
      | baseOp == 1, (a : b : c : remain) <- args = instruction "add" opcode [a, b, c] : go remain
      | baseOp == 2, (a : b : c : remain) <- args = instruction "mul" opcode [a, b, c] : go remain
      | baseOp == 3, (a : remain)         <- args = instruction "in"  opcode [a]       : go remain
      | baseOp == 4, (a : remain)         <- args = instruction "out" opcode [a]       : go remain
      | baseOp == 5, (a : b : remain)     <- args = instruction "bt"  opcode [a, b]    : go remain
      | baseOp == 6, (a : b : remain)     <- args = instruction "bf"  opcode [a, b]    : go remain
      | baseOp == 7, (a : b : c : remain) <- args = instruction "tlt" opcode [a, b, c] : go remain
      | baseOp == 8, (a : b : c : remain) <- args = instruction "teq" opcode [a, b, c] : go remain
      | baseOp == 9, (a : remain)         <- args = instruction "rel" opcode [a]       : go remain
      | baseOp == 99, remain <- args              = instruction "hlt" opcode []        : go remain
      where
        baseOp = opcode `mod` 100
    go (i : remain) = Line Nothing (Just $ Directive (D "data") [I i]) Nothing : go remain

instruction :: Text -> Int -> [Int] -> Line
instruction mnemonic opcode args =
  Line Nothing (Just instr) Nothing
  where
    instr = Instruction (M mnemonic) (zipModeFlags opcode args)

zipModeFlags :: Int -> [Int] -> [Arg]
zipModeFlags opcode args = zipWith toArg (modes opcode) args
  where
    toArg 1 value = ImmediateNumber value
    toArg 2 value = RelativeNumber value
    toArg _ value = Number value

digit :: Int -> Int -> Int
digit i value = value `div` (10 ^ i) `mod` 10

modes :: Int -> [Int]
modes opcode = map (\i -> digit i opcode) [2..4]

