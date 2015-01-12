module Main where

import Control.Monad.State
import Data.Map as Map
import Data.List

import Debug.Trace

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
import Flow

import MakeGraph
import Liveness
import Color

compile_frags :: [Frame.Frag] -> Temp.Temp -> String -> (Temp.Temp, String)
compile_frags [] t s = (t, s)
compile_frags (frg:frgs) t s =
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

    (cflow, nodes) = instrs2graph insts
    g = Flow.get_control cflow
    ds = Flow.get_def cflow
    us = Flow.get_use cflow
    ismove = Flow.get_ismove cflow
    outs = calcLiveness ds us g

    adjListMap = build nodes ds us ismove outs
    selectStack = [0..(Temp.temps t' - 1)]
    colmap = assignColor adjListMap selectStack

    -- number of local registers = maxreg + 1
    maxreg = head $ reverse $ sort $ Map.elems colmap

    mkString n | n < 0 = Temp.makeString n
               | otherwise =
                 case Map.lookup n colmap of
                   Just c -> "v" ++ show (c + 2)
                   _ -> error $ "mkString error: " ++ (show n) ++ "\n"

    str = concat $ fmap (A.format mkString) insts
    -- str = concat $ fmap (A.format Temp.makeString) insts

    hdr =
      ".method public static "
      ++ Frame.name frm
      ++ "([Ljava/lang/Object;Ljava/lang/Integer;)Ljava/lang/Integer;\n"
      ++ ".locals " ++ (show $ maxreg + 1)
      ++ "\n"
    

    ftr = "nop\n.end method\n"
  in
   compile_frags frgs t' (s ++ hdr ++ str ++ ftr)

printPreamble :: IO ()
printPreamble = do
  putStrLn ".class public Luhideyuki/daat/DaatProg;"
  putStrLn ".super Ljava/lang/Object;\n"
  putStrLn ".method public constructor <init>()V"
  putStrLn "    .registers 1"
  putStrLn "    invoke-direct {p0}, Ljava/lang/Object;-><init>()V"
  putStrLn "    return-void"
  putStrLn ".end method\n"

main = do
  s <- getContents
  let tokens = alexScanTokens s
  let ast = parse tokens
  let (ast', _, _) = findEscape ast
  let (ty, frgs, t) = transProg base_venv base_tenv ast'
  let (_, s) = compile_frags frgs t ""
  printPreamble
  putStrLn s

  
  

  
