module WhilePrograms where

import Data.Natural

import Store

-- While Programs
----------------------------------------------
data Program
    = Skip
    | Inc Natural
    | Dec Natural
    | Seq Program Program
    | IfZ Natural Program Program
    | While Natural Program
    deriving Show
    
    
-- Prettyprint
pretty_string :: Program -> RegisterSymbols 
    -> Int -> Int -> String
pretty_string pi syms indent lvl = 
    let spaces = replicate (lvl * indent) ' '
        str = case pi of
            Skip -> spaces ++ "Skip;\n"
            Inc i -> spaces ++ "Inc " ++ get_sym syms i ++ ";\n"
            Dec i -> spaces ++ "Dec " ++ get_sym syms i ++ ";\n"
            Seq p q ->
                let p' = pretty_string p syms indent lvl
                    q' = pretty_string q syms indent lvl
                in p' ++ q'
            IfZ i p q ->
                let ifz = spaces ++ "IfZ " ++ get_sym syms i ++ " {\n"
                    p' = pretty_string p syms indent $ lvl + 1
                    els = spaces ++ "} else {\n"
                    q' = pretty_string q syms indent $ lvl + 1
                    end = spaces ++ "}\n"
                in ifz ++ p' ++ els ++ q' ++ end
            While i p ->
                let begin = spaces ++ "While " ++ get_sym syms i ++ " {\n"
                    p' = pretty_string p syms indent $ lvl + 1
                    end = spaces ++ "}\n"
                in begin ++ p' ++ end
    in str

    
pretty_print :: Program -> RegisterSymbols -> IO ()
pretty_print p syms =
    let str = pretty_string p syms 2 0
    in mapM_ putStrLn $ lines str
    

-- Semantic
execute :: Program -> Store -> Store
execute pi s = case pi of
    Skip -> s
    Inc i -> set s i $ (s i) + 1
    Dec i -> set s i $ monus (s i) 1
    Seq p q -> execute q $ execute p s
    IfZ i p q -> if s i == 0 
        then execute p s
        else execute q s
    While i p -> if s i == 0
        then s
        else execute (Seq p pi) s

        
-- Free registers        
fresh_reg :: Program -> Natural
fresh_reg pi = case pi of
    Skip -> 1
    Inc i -> i + 1
    Dec i -> i + 1
    Seq p q -> max (fresh_reg p) (fresh_reg q)
    IfZ i p q -> maximum [i + 1, fresh_reg p, fresh_reg q]
    While i p -> max (i + 1) (fresh_reg p)
    

        
-- Runs p with specified input and returns output
run_with :: Program -> [Natural] -> Natural
run_with p input = output $ execute p $ from_input input

-- Runs p with specified input and returns 
-- content of registers up to n
debug_run_with :: Program -> [Natural] -> Natural -> [Natural]
debug_run_with p input n =
    let s = execute p $ from_input input
    in content_up_to s n


-- Composite Programs and Helper functions
----------------------------------------------

-- Helper functions
------------------------
seq_n :: [Program] -> Program
seq_n ps = case ps of
    [] -> Skip
    p:[] -> p
    p:ps -> Seq p $ seq_n ps

-- Zeroes the register
zero :: Natural -> Program
zero r = While r $ Dec r
    
-- Zeroes register "from" and adds its content
-- to register "to"
zero_and_add_to :: Natural -> Natural -> Program
zero_and_add_to from to 
    = While from $ Seq (Dec from) (Inc to)

-- Zeroes register "from" and adds its content
-- to the registers in the list "to"
zero_and_add_to_n :: Natural -> [Natural] -> Program
zero_and_add_to_n from to
    = While from $ Seq (Dec from) $ seq_n $ map Inc to
    
-- Adds the content of register "from" to register
-- "to", preserving it in "from" by temporarily
-- saving it in the free register "free"
add_to :: Natural -> Natural -> Natural -> Program
add_to from to free = 
    let add = zero_and_add_to_n from [to, free]
        restore = zero_and_add_to free from
    in Seq add restore

add_to_n :: Natural -> [Natural] -> Natural -> Program
add_to_n from to free =
    let add = zero_and_add_to_n from $ to ++ [free]
        restore = zero_and_add_to free from
    in Seq add restore
    
-- Copies the content of register "from" to register
-- "to" (zeroing it first), preserving it by
-- temporarily saving it in "free"    
copy_to :: Natural -> Natural -> Natural -> Program
copy_to from to free =
    let z = zero to
        copy = add_to from to free
    in Seq z copy
    
    
-- Sets the register to the specified number
set_reg :: Natural -> Natural -> Program
set_reg r n =
    let z = zero r
        s = seq_n $ replicate (fromIntegral n) $ Inc r
    in if n > 0 then Seq z s else z
        
    
-- Arithmetic on N^2
------------------------
plus :: Program
plus = Seq (zero_and_add_to 1 0) (zero_and_add_to 2 0)

times :: Program
times = While 1 $ Seq (Dec 1) $ add_to 2 0 3