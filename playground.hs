-- TODO: Implement minus, division and error message when doing division with zero

-- An arithmetic expression
data Expr = Lit Int | Plus Expr Expr | Times Expr Expr deriving Show

-- Interprets an arithmetic expression
interpret :: Expr -> Int

interpret (Lit a) = a

interpret (Plus a b) = (interpret a) + (interpret b)

interpret (Times a b) = (interpret a) * (interpret b)

-- Bytecode is what our stack is made of
data Bytecode = Push Int | Add | Mul deriving Show

-- The datastack contains the information pushed to it by Bytecodes
type Datastack = [Int]

-- Executing the Bytecodes
execute_byte_code :: Bytecode -> Datastack -> Datastack

execute_byte_code (Push a) stack = a : stack

execute_byte_code Add (x : y : stack) = (x + y) : stack
execute_byte_code Add (x : stack) = x : stack
execute_byte_code Add [] = 0 : []

execute_byte_code Mul (x : y : stack) = (x * y) : stack
execute_byte_code Mul (x : stack) = x : stack
execute_byte_code Mul [] = 0 : []

-- A program is a list of bytecodes manipulating our datastack
type Bytecodeprogram = [Bytecode]

-- Takes byte_code_program and a stack and returns a stack
execute_byte_code_program :: Bytecodeprogram -> Datastack -> Datastack

execute_byte_code_program (p1 : bytecodes) stack = execute_byte_code_program bytecodes (execute_byte_code p1 stack)
execute_byte_code_program [] stack = stack

-- Two different kinds of compiles.
-- A compilation of an arithmetic expression gives us a Bytecode prorgram to be executed
compile_plusplus :: Expr ->  Bytecodeprogram

compile_plusplus (Lit n) = (Push n : [])
compile_plusplus (Plus e1 e2) = (compile_plusplus e1) ++ (compile_plusplus e2) ++ (Add : [])
compile_plusplus (Times e1 e2) = (compile_plusplus e1) ++ (compile_plusplus e2) ++ (Mul : [])

compile_acc :: Expr -> Bytecodeprogram -> Bytecodeprogram
compile_acc (Lit n) acc = (Push n : acc)
compile_acc (Plus e1 e2) acc = compile_acc e2 (compile_acc e1 (Add : acc))
compile_acc (Times e1 e2) acc = compile_acc e2 (compile_acc e1 (Mul : acc))

-- Compare the execution of the two different kinds of compilation
execute_success :: Expr -> Bool
execute_success expr = execute_byte_code_program (compile_acc expr []) [] == execute_byte_code_program (compile_plusplus expr) []

-- We can now give this function to another function "polymorphically" equal to the other compile function
compile_acc_v1 :: Expr -> Bytecodeprogram
compile_acc_v1 expr = compile_acc expr []

-- Compare the interpret and the execution of compile
execute_and_interpret :: Expr -> Bool
execute_and_interpret expr = interpret expr == execute_byte_code_program (compile_plusplus expr) [] !! 0
-- TODO: Implement the check_entry: Seeing that there is only one extra element on the stack after running an arithmetic expression
