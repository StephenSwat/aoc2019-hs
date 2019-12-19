module Intcode where

import Data.IntMap (IntMap, insert, fromList, findWithDefault, empty, findMax)
    
type Tape = IntMap Int
    
data OperationType = Add | Multiply | Write | Read | BranchTrue | BranchFalse | LessThan | Equals | AdjustBase | Exit deriving (Show, Eq)
data OperandMode = Immediate | Reference | Relative deriving (Show, Eq)

data Operation = Operation {
    opcode :: OperationType,
    modes :: [OperandMode]
} deriving Show

data IntcodeState = IntcodeState {
    tape   :: Tape,
    output :: [Int],
    input  :: [Int],
    pc     :: Int,
    base   :: Int,
    halt   :: Bool
}

defaultState = IntcodeState{tape=empty, output=[], input=[], pc=0, base=0, halt=False}

fromTapeList :: [Int] -> IntcodeState
fromTapeList x = defaultState{tape=fromList (zip [0..] x)}

toTapeList :: IntcodeState -> [Int]
toTapeList IntcodeState{tape=t} = map (\x -> findWithDefault 0 x t) [0..m]
    where
        (m, _) = findMax t

initIntcode :: [Int] -> [Int] -> IntcodeState
initIntcode x i = defaultState{tape=fromList (zip [0..] x), input=i}

replace :: Tape -> Int -> Int -> Tape
replace l i n = insert i n l

opCode :: Int -> OperationType
opCode 1  = Add
opCode 2  = Multiply
opCode 3  = Write
opCode 4  = Read
opCode 5  = BranchTrue
opCode 6  = BranchFalse
opCode 7  = LessThan
opCode 8  = Equals
opCode 9  = AdjustBase
opCode 99 = Exit
opCode x  = error ("Invalid operation: " ++ (show x))

opMode :: Int -> OperandMode
opMode 0 = Reference
opMode 1 = Immediate
opMode 2 = Relative
opMode x = error ("Invalid operand mode: " ++ (show x))

arguments :: OperationType -> Int
arguments Add         = 3
arguments Multiply    = 3
arguments Write       = 1
arguments Read        = 1
arguments BranchTrue  = 2
arguments BranchFalse = 2
arguments LessThan    = 3
arguments Equals      = 3
arguments AdjustBase  = 1
arguments Exit        = 0 

operation :: Int -> Operation
operation x = Operation{opcode=opcode, modes=modes}
    where 
        (q, r) = x `divMod` 100
        opcode = opCode r
        modes = map (opMode . (`mod` 10) . (q `div`) . (10^)) [0..arguments opcode - 1]

getOperation :: [Int] -> (Operation, [Int])
getOperation (i:xs)
    | (length xs) >= nargs = (op, take nargs xs)
    | otherwise = error "Insufficient arguments!"
    where 
        op = operation i
        nargs = arguments (opcode op)

nextOpcode :: IntcodeState -> OperationType
nextOpcode IntcodeState{tape=t, pc=c} = q
    where
        (Operation{opcode=q}, _) = getOperation (getRun t [c..c+4])
        
getArgValue :: (OperandMode, Int) -> IntcodeState -> Int
getArgValue i@(m, x) s
    | m == Immediate = x
    | otherwise = findWithDefault 0 p (tape s)
    where
        p = getArgPosition i s

getArgPosition :: (OperandMode, Int) -> IntcodeState -> Int
getArgPosition (m, x) s
    | m == Immediate = 0
    | m == Reference = x
    | m == Relative  = x + base s

getRun :: Tape -> [Int] -> [Int]
getRun t v = map (\x -> findWithDefault 0 x t) v

addInput :: IntcodeState -> [Int] -> IntcodeState
addInput s@IntcodeState{input=oi} i = s{input=oi ++ i} 

takeOutput :: IntcodeState -> Int -> (IntcodeState, [Int])
takeOutput s@IntcodeState{output=o} n = (s{output=rem}, tk)
    where
        (tk, rem) = splitAt n o
        
takeAllOutput :: IntcodeState -> (IntcodeState, [Int])
takeAllOutput s@IntcodeState{output=o} = (s{output=[]}, o)

stepProgram :: IntcodeState -> IntcodeState
stepProgram s@IntcodeState{tape=x, pc=c, input=i, output=o, base=b, halt=h} = IntcodeState{tape=tape, pc=pc, input=input, output=output, base=base, halt=halt}
    where 
        (Operation{opcode=op,modes=modes}, args) = getOperation (getRun x [c..c+4])
        argv = map (flip getArgValue s) (zip modes args) 
        argp = map (flip getArgPosition s) (zip modes args) 
        tape = case op of
            Add         -> replace x (argp !! 2) ((argv !! 0) + (argv !! 1))
            Multiply    -> replace x (argp !! 2) ((argv !! 0) * (argv !! 1))
            Write       -> replace x (argp !! 0) (head i)
            LessThan    -> replace x (argp !! 2) (if (argv !! 0) < (argv !! 1) then 1 else 0)
            Equals      -> replace x (argp !! 2) (if (argv !! 0) == (argv !! 1) then 1 else 0)
            _           -> x
        input = case op of
            Write       -> tail i
            _           -> i
        output = case op of
            Read        -> o ++ [argv !! 0]
            _           -> o
        pc = case op of
            BranchTrue  -> if argv !! 0 /= 0 then argv !! 1 else c + 3
            BranchFalse -> if argv !! 0 == 0 then argv !! 1 else c + 3
            x           -> c + (arguments x) + 1
        base = case op of
            AdjustBase  -> b + (argv !! 0)
            _           -> b
        halt = case op of
            Exit        -> True
            _           -> False

runProgramUntil :: (IntcodeState -> Bool) -> IntcodeState -> IntcodeState
runProgramUntil f s
    | (f s) = s
    | otherwise = runProgramUntil f (stepProgram s)
    
runProgramUntilOutput :: IntcodeState -> IntcodeState
runProgramUntilOutput = runProgramUntil (\IntcodeState{output=o, halt=h} -> h || not (null o))

runProgramUntilNeedInput :: IntcodeState -> IntcodeState
runProgramUntilNeedInput = runProgramUntil (\s@IntcodeState{input=i, halt=h} -> h || (null i && nextOpcode s == Write))
            
runProgram :: IntcodeState -> IntcodeState
runProgram s = runProgramUntil halt s
