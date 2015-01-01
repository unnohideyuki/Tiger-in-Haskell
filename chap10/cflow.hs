import Control.Monad.State

import Parser (parse)
import Lexer (alexScanTokens, prettyToken)
import Semant
import Env
import FindEscape
import qualified Canon as C
import DalvikFrame as Frame
import qualified Assem as A
import qualified Codegen as CG
import qualified Temp

import MakeGraph

compile_frags :: [Frame.Frag] -> Temp.Temp -> [[A.Instr]] -> (Temp.Temp, [[A.Instr]])
compile_frags [] t is = (t, is)
compile_frags (frg:frgs) t is =
  let
    stm = Frame.get_body frg
    frm = Frame.get_frame frg
    (stms1, t1) = C.linearize stm t
    (stms2, t2, lab) = C.basicBlocks stms1 t1
    (stms3, t3) = C.traceSchedule stms2 lab t2
    
    cgstat = CG.CgenState [] t3 frm
    (_, cgstat') = runState (CG.munchStm stms3) cgstat
                            
    t' = CG.get_temp cgstat'
    insts = CG.get_insts cgstat'
  in
   compile_frags frgs t' (is ++ [insts])

printPreamble :: IO ()
printPreamble = do
  putStrLn ".class public Luhideyuki/daat/DaatProg;"
  putStrLn ".super Ljava/lang/Object;\n"
  putStrLn ".method public constructor <init>()V"
  putStrLn "    .registers 1"
  putStrLn "    invoke-direct {p0}, Ljava/lang/Object;-><init>()V"
  putStrLn "    return-void"
  putStrLn ".end method\n"

cflow :: [[A.Instr]] -> IO ()
cflow [] = return ()
cflow (instrs:is) = do
  print $ instrs2graph instrs
  cflow is

main = do
  s <- getContents
  let tokens = alexScanTokens s
  let ast = parse tokens
  let (ast', _, _) = findEscape ast
  let (ty, frgs, t) = transProg base_venv base_tenv ast'
  let (_, is) = compile_frags frgs t []
  print is
  cflow is

  
  

  
