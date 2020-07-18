{-# LANGUAGE NamedFieldPuns, OverloadedLists, OverloadedStrings #-}

module Intcode.Assembler (assemble, AssembleError (..)) where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State.Strict (State, execState, get, gets, modify, put, runState)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_, foldl')
import           Data.Function ((&))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import           Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as Vector

import           Intcode.Syntax

data AssembleError
  = UndefinedLabel Text
  | DuplicateLabel Text
  | InvalidMnemonic Text
  | InvalidArgs Text [Text]
  | InvalidArgMode
  | InvalidDirective Text
  | InvalidDirectiveArgs Text [Text]
  deriving (Eq, Show)

-- | Assemble source code into machine code.
assemble :: [Line] -> Either [AssembleError] (Vector Int)
assemble input =
  if null cgErrors
    then Right (Vector.take cgSize cgOutput)
    else Left cgErrors
  where
    CodeGenState { cgSize, cgOutput, cgErrors } = execCodeGen (mapM assembleLine input >> link) emptyCodeGenState

-- | Assemble a source code line.
--
-- Lines contain a label, an instruction or directive, and a comment (all
-- optional).
assembleLine :: Line -> CodeGen ()
assembleLine (Line label instructionOrDirective _) = do
  for_ label $ \(L name) -> defineLabel name
  for_ instructionOrDirective assembleInstructionOrDirective
  where
    defineLabel name = do
      defined <- lookupLabel name
      case defined of
        Just _  -> addError (DuplicateLabel name)
        Nothing -> addLabel name
    assembleInstructionOrDirective iod =
      case iod of
        Instruction mnemonic args -> assembleInstruction mnemonic args
        Directive name args -> applyDirective name args

-- | Assemble a machine instruction.
assembleInstruction :: Mnemonic -> [Arg] -> CodeGen ()
assembleInstruction (M "add") args = assembleBinOp  "add" 1 args
assembleInstruction (M "mul") args = assembleBinOp  "mul" 2 args
assembleInstruction (M "in")  args = assembleInput  "in"  3 args
assembleInstruction (M "out") args = assembleOutput "out" 4 args
assembleInstruction (M "bt")  args = assembleJump   "bt"  5 args
assembleInstruction (M "bf")  args = assembleJump   "bf"  6 args
assembleInstruction (M "tlt") args = assembleBinOp  "tlt" 7 args
assembleInstruction (M "teq") args = assembleBinOp  "teq" 8 args
assembleInstruction (M "rel") args = assembleRel    "rel" 9 args
assembleInstruction (M "hlt") args = assembleHlt    "hlt" 99 args
assembleInstruction (M mnemonic) _ = addError (InvalidMnemonic mnemonic)

-- | Assemble a binary instruction (three arguments).
--
-- The instruction is assembled in a block that will abort if the third
-- argument, the destination address, is an immediate mode argument.
assembleBinOp :: Text -> Int -> [Arg] -> CodeGen ()
assembleBinOp mnemonic baseOp args
  | [a, b, c] <- args = block $ do
    assembleOpcode baseOp args
    assembleArg Read a
    assembleArg Read b
    assembleArg Write c
  | otherwise = addError (InvalidArgs mnemonic ["R", "R", "a/r"])

-- | Assemble an input instruction.
assembleInput :: Text -> Int -> [Arg] -> CodeGen ()
assembleInput mnemonic baseOp args
  | [a] <- args = block $ do
    assembleOpcode baseOp args
    assembleArg Write a
  | otherwise = addError (InvalidArgs mnemonic ["a/r"])

-- | Assemble an output instruction.
assembleOutput :: Text -> Int -> [Arg] -> CodeGen ()
assembleOutput mnemonic baseOp args
  | [a] <- args = block $ do
    assembleOpcode baseOp args
    assembleArg Read a
  | otherwise = addError (InvalidArgs mnemonic ["R"])

-- | Assemble a jump instruction.
assembleJump :: Text -> Int -> [Arg] -> CodeGen ()
assembleJump mnemonic baseOp args
  | [a, b] <- args = block $ do
    assembleOpcode baseOp args
    assembleArg Read a
    assembleArg Read b
  | otherwise = addError (InvalidArgs mnemonic ["R", "R"])

-- | Assemble a set-relative-base instruction.
assembleRel :: Text -> Int -> [Arg] -> CodeGen ()
assembleRel mnemonic baseOp args
  | [a] <- args = block $ do
    assembleOpcode baseOp args
    assembleArg Read a
  | otherwise = addError (InvalidArgs mnemonic ["R"])

-- | Assemble a halt instruction.
assembleHlt :: Text -> Int -> [Arg] -> CodeGen ()
assembleHlt _ op [] = emit [op]
assembleHlt mnemonic _ _ = addError (InvalidArgs mnemonic [])

data OperandType = Read | Write

-- | Assemble an opcode, including argument mode flags.
assembleOpcode :: Int -> [Arg] -> CodeGen ()
assembleOpcode baseOp args = do
  let op = baseOp + modeFlags args
  emit [op]

-- | Assemble an argument (operand).
--
-- Arguments referring to labels are emitted as placeholders and added to the
-- list of references.
--
-- Not all operands support all argument types. For example, a target memory
-- location operand does not accept immediate arguments. If the type of
-- argument is not valid for the operand, the block is aborted.
assembleArg :: OperandType -> Arg -> CodeGen ()
assembleArg _     (Number value)          = emit [value]
assembleArg Read  (ImmediateNumber value) = emit [value]
assembleArg Write (ImmediateNumber _)     = addError InvalidArgMode >> abort
assembleArg _     (RelativeNumber value)  = emit [value]
assembleArg _     (Label (L label))       = addReference label >> emit [-999]

-- | Mode flags for arguments.
modeFlags :: [Arg] -> Int
modeFlags args = foldl' pushFlag 0 $ zip [0..] args
  where
    pushFlag :: Int -> (Int, Arg) -> Int
    pushFlag modes (i, arg) =
      case arg of
        Number _          -> modes -- 0
        ImmediateNumber _ -> modes + 10^(i + 2) -- 1
        RelativeNumber  _ -> modes + 2 * 10^(i + 2) -- 2
        Label _           -> modes + 10^(i + 2) -- 1 (no indirect jumps)

-- | Apply the effects of a directive.
applyDirective :: Directive -> [Literal] -> CodeGen ()
applyDirective (D "org") [I pos] = seek pos
applyDirective (D "org") _ = addError (InvalidDirectiveArgs "org" ["address"])
applyDirective (D directive) _ = addError (InvalidDirective directive)

-- | Resolve references to labels by replacing them with their addresses.
--
-- Unresovled references are accumulated in the errors and not substituted.
link :: CodeGen ()
link = do
  CodeGenState { cgOutput, cgLabels, cgReferences } <- get
  let resolve (index, labelName) =
        case HashMap.lookup labelName cgLabels of
          Nothing   -> Left labelName
          Just addr -> Right (index, addr)
      (undefinedLabels, substitutions) = cgReferences & HashMap.toList & map resolve & partitionEithers
  for_ undefinedLabels (addError . UndefinedLabel)
  modify $ \s -> s { cgOutput = cgOutput // substitutions, cgReferences = [] }

-- * Code generation monad

-- | Move the write head to the specified address. The next @emit@ will write
-- here.
seek :: Int -> CodeGen ()
seek pos = modify $ \s -> s { cgPos = pos }

-- | Ensure there are @n@ cells ahead of the current write head. If the output
-- buffer is resized, it is filled with zeroes.
ensure :: Int -> CodeGen ()
ensure n = modify $ \s@CodeGenState { cgPos, cgOutput } ->
    if Vector.length cgOutput < cgPos + n
      then s { cgOutput = cgOutput <> Vector.replicate 256 0 }
      else s

-- | Emit some opcodes to the output at the current write head.
-- The output buffer is resized if necessary.
emit :: [Int] -> CodeGen ()
emit xs = do
  ensure (length xs)
  modify $ \s@CodeGenState { cgPos, cgSize, cgOutput } ->
    let newPos = cgPos + length xs
    in  s { cgPos = newPos
          , cgSize = max cgSize newPos
          , cgOutput = cgOutput // zip [cgPos ..] xs
          }

-- | Register a label for the current write head. The linker uses this later
-- to resolve references.
--
-- An existing label with this name is replaced.
addLabel :: Text -> CodeGen ()
addLabel name = modify $ \s@CodeGenState { cgPos, cgLabels } ->
  s { cgLabels = HashMap.insert name cgPos cgLabels }

lookupLabel :: Text -> CodeGen (Maybe Int)
lookupLabel name = gets (HashMap.lookup name . cgLabels)

-- | Record a reference at the position of the write head for the linker to
-- resolve.
addReference :: Text -> CodeGen ()
addReference labelName = modify $ \s@CodeGenState { cgPos, cgReferences } ->
  s { cgReferences = HashMap.insert cgPos labelName cgReferences }

-- | Add an error.
addError :: AssembleError -> CodeGen ()
addError err = modify $ \s@CodeGenState { cgErrors } ->
  s { cgErrors = err : cgErrors }

-- | Save the code generator state and execute a block. If the block is
-- aborted, all state except errors is reverted. Errors added in the block
-- remain.
block :: CodeGen a -> CodeGen ()
block action = do
  saved@CodeGenState { cgErrors = savedErrors } <- get
  case runState (runExceptT action) saved of
    (Left (), CodeGenState { cgErrors = newErrors }) -> put saved { cgErrors = newErrors ++ savedErrors }
    (Right _, newState) -> put newState

-- | Abort a block. All state except errors are reverted. Errors added in the
-- block remain.
abort :: CodeGen a
abort = throwError ()

data CodeGenState = CodeGenState
  { cgPos          :: Int
  , cgSize         :: Int
  , cgOutput       :: Vector Int
  , cgLabels       :: HashMap Text Int
  , cgReferences   :: HashMap Int Text
  , cgErrors       :: [AssembleError]
  }

emptyCodeGenState :: CodeGenState
emptyCodeGenState = CodeGenState 0 0 (Vector.replicate 256 0) [] [] []

type CodeGen a = ExceptT () (State CodeGenState) a

execCodeGen :: CodeGen a -> CodeGenState -> CodeGenState
execCodeGen action initialState = execState (runExceptT action) initialState
