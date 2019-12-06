module Common where

data OperationType = Add | Multiply | Write | Read | BranchTrue | BranchFalse | LessThan | Equals | Exit deriving (Show, Eq)
data OperandMode = Immediate | Reference deriving (Show, Eq)

data Operation = Operation {
    opcode :: OperationType,
    modes :: [OperandMode]
} deriving Show

data State = State {
    tape   :: [Int],
    output :: [Int],
    input  :: [Int],
    pc     :: Int
}
        
replace :: [Int] -> Int -> Int -> [Int]
replace l i n = take i l ++ [n] ++ drop (i + 1) l

opCode :: Int -> OperationType
opCode 1  = Add
opCode 2  = Multiply
opCode 3  = Write
opCode 4  = Read
opCode 5  = BranchTrue
opCode 6  = BranchFalse
opCode 7  = LessThan
opCode 8  = Equals
opCode 99 = Exit
opCode x  = error ("Invalid operation: " ++ (show x))

opMode :: Int -> OperandMode
opMode 0 = Reference
opMode 1 = Immediate
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
        
getArgValue :: (OperandMode, Int) -> [Int] -> Int
getArgValue (Immediate, x) _ = x
getArgValue (Reference, x) l = l !! x
        
runProgram :: State -> State
runProgram State{tape=x, pc=c, input=i, output=o}
    | op == Exit = State{tape=x, pc=c, input=i, output=o}
    | otherwise = runProgram State{tape=tape, pc=pc, input=input, output=output}
    where 
        (Operation{opcode=op,modes=modes}, args) = (getOperation . drop c) x
        argv = map (flip getArgValue x) (zip modes args) 
        tape = case op of
            Add         -> (replace x (args !! 2) ((argv !! 0) + (argv !! 1)))
            Multiply    -> (replace x (args !! 2) ((argv !! 0) * (argv !! 1)))
            Write       -> (replace x (args !! 0) (head i))
            LessThan    -> (replace x (args !! 2) (if (argv !! 0) < (argv !! 1) then 1 else 0))
            Equals      -> (replace x (args !! 2) (if (argv !! 0) == (argv !! 1) then 1 else 0))
            _           -> x
        input = case op of
            Write       -> tail i
            _           -> i
        output = case op of
            Read        -> o ++ [(argv !! 0)]
            _           -> o
        pc = case op of
            BranchTrue  -> if argv !! 0 /= 0 then argv !! 1 else c + 3
            BranchFalse -> if argv !! 0 == 0 then argv !! 1 else c + 3
            x           -> c + (arguments x) + 1