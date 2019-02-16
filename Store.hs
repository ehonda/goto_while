module Store where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Natural

-- Store
----------------------------------------------
type Store = Natural -> Natural
type RegisterSymbols = M.Map Natural String

default_symbols :: RegisterSymbols
default_symbols = M.empty

get_sym :: RegisterSymbols -> Natural -> String
get_sym syms n = fromMaybe (show n) $ M.lookup n syms

set_syms :: RegisterSymbols -> [(Natural, String)] -> RegisterSymbols
set_syms smys kv_list =
    let op = \r kv -> M.insert (fst kv) (snd kv) r
    in foldl op smys kv_list

set_sym :: RegisterSymbols -> (Natural, String) -> RegisterSymbols
set_sym syms kv = set_syms syms [kv]


math_symbols1 :: RegisterSymbols
math_symbols1 = set_syms default_symbols 
    [(0, "n"), (1, "x")]
    
math_symbols2 :: RegisterSymbols
math_symbols2 = set_sym math_symbols1 (2, "y")


empty :: Store
empty = \i -> 0

set :: Store -> Natural -> Natural -> Store
set s r n = \i -> if i == r then n else s i

content_up_to :: Store -> Natural -> [Natural]
content_up_to s n = map s [0..n]
        
from_reversed_input :: [Natural] -> Store
from_reversed_input input = case input of
    [] -> empty
    n:ns -> let
        s = from_reversed_input ns
        i = fromIntegral $ (length ns) + 1
        in set s i n
        
from_input :: [Natural] -> Store
from_input input = from_reversed_input $ reverse input

output :: Store -> Natural
output s = s 0
