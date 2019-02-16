module Transpile where

import Data.Natural

import qualified GotoPrograms as G
import Store
import qualified WhilePrograms as W

-- Transpilation While -> Goto
----------------------------------------------

trans_sub_w :: G.Address -> W.Program -> (G.Address, G.Program)
trans_sub_w a pi = case pi of
    W.Skip -> (a, [])
    W.Inc i -> (a + 1, [G.Inc i])
    W.Dec i -> (a + 1, [G.Dec i])
    W.Seq p q ->
        let (m, p') = trans_sub_w a p
            (e, q') = trans_sub_w m q
        in (e, p' ++ q')
    W.IfZ i p q ->
        let (m, q') = trans_sub_w (a + 1) q
            (e, p') = trans_sub_w (m + 1) p
        in (e, [G.GotoZ i (m + 1)] ++ q' ++ [G.Goto e] ++ p')
    W.While i p ->
        let (e, p') = trans_sub_w (a + 1) p
        in (e + 1, [G.GotoZ i (e + 1)] ++ p' ++ [G.Goto a])


transpile_w :: W.Program -> G.Program
transpile_w p =
    let (_, p') = trans_sub_w 0 p
    in p' ++ [G.Stop]
    
-- Transpilation Goto -> While
----------------------------------------------

-- Transpilation of a goto command
--  p   - Cmd to transpile
--  c   - Program counter register
--  h   - Halt register
trans_cmd_g :: G.Command -> Natural -> Natural -> W.Program
trans_cmd_g p c h = case p of
    G.Inc i -> W.Seq (W.Inc i) (W.Inc c)
    G.Dec i -> W.Seq (W.Dec i) (W.Inc c)
    G.GotoZ i k -> W.IfZ i (W.set_reg c k) (W.Inc c)
    G.Goto k -> W.set_reg c k
    G.Stop -> W.Dec h


-- Builds the branches in the translation
-- c' is decreased once every branch, starting at c
prepend_cmd :: W.Program -> W.Program -> Natural -> W.Program
prepend_cmd p q c' = W.IfZ c' p $ W.Seq (W.Dec c') q


transpile_g :: G.Program -> W.Program
transpile_g p =
    let c = G.fresh_reg p
        c' = c + 1
        h = c + 2
        tmp = c + 3
        loop_init = \q -> W.Seq (W.add_to c c' tmp) q
        main_body = \q -> W.Seq (W.Inc h) $ W.While h $ loop_init q
        p' = map (\p -> trans_cmd_g p c h) p
        op = \p q -> prepend_cmd p q c'
        loop_body = foldr op W.Skip p'
    in main_body loop_body

    
pretty_print_trans_g :: G.Program -> RegisterSymbols -> IO ()
pretty_print_trans_g p syms =
    let c = G.fresh_reg p
        c' = c + 1
        h = c + 2
        tmp = c + 3
        syms' = set_syms syms 
            [(c, "c"), (c', "c'"), (h, "h"), (tmp, "tmp")]
        p' = transpile_g p
    in W.pretty_print p' syms'