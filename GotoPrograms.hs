module GotoPrograms where

import Control.Monad
import Data.Natural

import Store

-- Goto Programs
----------------------------------------------
type Address = Natural

data Command
    = Inc Natural
    | Dec Natural
    | Goto Address
    | GotoZ Natural Address
    | Stop
    deriving Show

    
type Program = [Command]
type Configuration = (Address, Store)


-- Prettyprint
pretty_str_cmd :: Command -> RegisterSymbols -> String
pretty_str_cmd c syms = case c of
    Inc i -> "Inc " ++ get_sym syms i
    Dec i -> "Dec " ++ get_sym syms i
    Goto k -> "Goto " ++ show k
    GotoZ i k -> "GotoZ " ++ get_sym syms i ++ " " ++ show k
    Stop -> "Stop"

pretty_lines :: Program -> RegisterSymbols -> [String]
pretty_lines p syms =
    let pretty_cmds = map (\c -> pretty_str_cmd c syms) p
        indices = map show [0..((length p) - 1)]
        line_op = \i c -> i ++ ": " ++ c
    in zipWith line_op indices pretty_cmds

pretty_print :: Program -> RegisterSymbols -> IO ()
pretty_print p syms = mapM_ putStrLn $ pretty_lines p syms
        
-- Semantic, Relation step_p
step :: Program -> Configuration -> Configuration
step p (l, s) =
    if (fromIntegral l) >= length p
    then undefined
    else
        case (p !! (fromIntegral l)) of
            Inc i ->
                let s' = set s i $ (s i) + 1
                    l' = l + 1
                in (l', s')
            Dec i ->
                let s' = set s i $ monus (s i) 1
                    l' = l + 1
                in (l', s')
            Goto k -> (k, s)
            GotoZ i k -> if s i == 0 then (k, s) else (l + 1, s)
            Stop -> (l, s)


is_final :: Program -> Configuration -> Bool
is_final p (l, s) =
    let li = fromIntegral l
    in 
        if li >= length p
        then False
        else case (p !! li) of
            Stop -> True
            _ -> False
            

-- Relation step_p^*
execute :: Program -> Configuration -> Configuration
execute p (l, s) =
    let (l', s') = step p (l, s)
    in
        if is_final p (l', s')
        then (l', s')
        else execute p (l', s')
        
        
-- Free registers
fresh_reg_cmd :: Command -> Natural
fresh_reg_cmd c = case c of
    Inc i -> i + 1
    Dec i -> i + 1
    Goto _ -> 1
    GotoZ i _ -> i + 1
    Stop -> 1
    
fresh_reg :: Program -> Natural
fresh_reg p = maximum $ map fresh_reg_cmd p
        
        
-- Runs p with specified input and returns output
run_with :: Program -> [Natural] -> Natural
run_with p input = output $ snd $ execute p (0, from_input input)


-- Runs p with specified input and returns 
-- content of registers up to n
debug_run_with :: Program -> [Natural] -> Natural -> [Natural]
debug_run_with p input n =
    let s = snd $ execute p (0, from_input input)
    in content_up_to s n
    
    
-- Composite Programs and Helper functions
----------------------------------------------

-- Helper functions
------------------------
zero_and_add_to :: Address -> Natural -> Natural -> Program
zero_and_add_to offset from to
    = [GotoZ from (4 + offset), Dec from, Inc to, Goto offset]
    
zero_and_add_to_n :: Address -> Natural -> [Natural] -> Program
zero_and_add_to_n offset from to =
    let incs = map Inc to
        end = fromIntegral $ 3 + length incs
    in [GotoZ from (end + offset), Dec from] ++ incs ++ [Goto offset]
    
add_to :: Address -> Natural -> Natural -> Natural -> Program
add_to offset from to free =
    let add = zero_and_add_to_n offset from [to, free]
        off_restore = offset + (fromIntegral (length add))
        restore = zero_and_add_to off_restore free from
    in add ++ restore

-- Arithmetic on N^2
------------------------
plus :: Program
plus = 
    let p = zero_and_add_to 0 1 0
        q = zero_and_add_to (fromIntegral (length p)) 2 0
    in p ++ q ++ [Stop]
    
times :: Program
times =
    let add_y = add_to 2 2 0 3
        end_add_y = fromIntegral $ 2 + length add_y
    in [GotoZ 1 (end_add_y + 1), Dec 1] ++ add_y ++ [Goto 0, Stop]