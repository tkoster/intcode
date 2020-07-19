{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Char (isSpace)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Data.Traversable (sequence)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           System.Environment (getArgs)
import           System.IO (Handle, IOMode(ReadMode, WriteMode), hPutStrLn, stderr, stdin, stdout, withFile)

import           Intcode.Assembler
import           Intcode.Disassembler
import           Intcode.Pretty
import           Intcode.Syntax

main :: IO ()
main = do
  args <- getArgs
  mainWith cliAssemble Nothing Nothing args

mainWith :: (Handle -> Handle -> IO ()) -> Maybe Handle -> Maybe Handle -> [String] -> IO ()
mainWith action hin hout [] =
  action (fromMaybe stdin hin) (fromMaybe stdout hout)
mainWith _ _ _ ("-h" : _) = usage
mainWith _ _ _ ("--help" : _) = usage
mainWith _ hin hout ("-d" : remain) =
  mainWith cliDisassemble hin hout remain
mainWith action hin hout ("-o" : outpath : remain)
  | Nothing <- hout =
      withFile outpath WriteMode $ \hout' ->
        mainWith action hin (Just hout') remain
  | otherwise = usage
mainWith action hin hout (inpath : remain)
  | Nothing <- hin =
      withFile inpath ReadMode $ \hin' ->
        mainWith action (Just hin') hout remain
  | otherwise = usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  intcode FILE [-o FILE]"
  putStrLn "  intcode -d FILE [-o FILE]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  FILE     Read input from FILE instead of stdin."
  putStrLn "  -o FILE  Write output to FILE instead of stdout."
  putStrLn "  -d       Disassemble the input."

cliAssemble :: Handle -> Handle -> IO ()
cliAssemble hin hout = do
  input <- Text.hGetContents hin
  case parse input of
    Left err -> hPutStrLn stderr (show err)
    Right source ->
      case assemble source of
        Left err -> hPutStrLn stderr (show err)
        Right program -> hPutStrLn hout (show program)

cliDisassemble :: Handle -> Handle -> IO ()
cliDisassemble hin hout = do
  input <- Text.hGetContents hin
  case readIntcode input of
    Just ints -> do
      let source = pretty (disassemble ints)
      Text.hPutStr hout source
    Nothing -> hPutStrLn stderr "Invalid intcode."

readIntcode :: Text -> Maybe (Vector Int)
readIntcode input = do
  let cells = Text.splitOn "," input
      ints  = map readInt cells
  fmap Vector.fromList (sequence ints)
  where
    readInt s =
      case Text.signed Text.decimal (Text.dropWhile isSpace s) of
        Right (i, remain) | Text.all isSpace remain -> Just i
        _ -> Nothing
