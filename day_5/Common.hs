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
        opcode = opCode (x `mod` 100)
        modes = map (\y -> opMode ((x `div` (10^(y+1))) `mod` 10)) [1..arguments opcode]

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
runProgram State {tape=x, pc=c, input=i, output=o}
    | op == Add      = runProgram State{tape=(replace x (args !! 2) ((argv !! 0) + (argv !! 1))), pc=c + 4, input=i, output=o}
    | op == Multiply = runProgram State{tape=(replace x (args !! 2) ((argv !! 0) * (argv !! 1))), pc=c + 4, input=i, output=o}
    | op == Write    = runProgram State{tape=(replace x (args !! 0) (head i)), pc=c + 2, input=tail i, output=o}
    | op == Read     = runProgram State{tape=x, pc=c + 2, input=i, output=o ++ [(argv !! 0)]}
    | op == BranchTrue = runProgram State{tape=x, pc=if argv !! 0 /= 0 then argv !! 1 else c + 3, input=i, output=o}
    | op == BranchFalse = runProgram State{tape=x, pc=if argv !! 0 == 0 then argv !! 1 else c + 3, input=i, output=o}
    | op == LessThan    = runProgram State{tape=(replace x (args !! 2) (if (argv !! 0) < (argv !! 1) then 1 else 0)), pc=c + 4, input=i, output=o}
    | op == Equals    = runProgram State{tape=(replace x (args !! 2) (if (argv !! 0) == (argv !! 1) then 1 else 0)), pc=c + 4, input=i, output=o}
    | op == Exit     = State {tape=x, pc=c, input=i, output=o}
    | otherwise      = error "Bad input!"
    where 
        (Operation{opcode=op,modes=modes}, args) = (getOperation . drop c) x
        argv = map (\y -> getArgValue y x) (zip modes args) 
