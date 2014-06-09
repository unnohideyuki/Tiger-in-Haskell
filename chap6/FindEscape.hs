module FindEscape where

import Data.List
import Data.Foldable (foldr')

import qualified Absyn as A
import qualified Symbol as S

{- findEscape returns a pair of following:
    - Appropriately escape flaged AST
    - List of free variables
-}


findEscape :: A.Exp -> (A.Exp, [S.Symbol])

findEscape =
  let
    findesc exp@A.NilExp = (exp, [])
    
    findesc exp@(A.IntExp _ _) = (exp, [])
    
    findesc exp@(A.StringExp _ _) = (exp, [])
    
    findesc exp@A.OpExp {A.lhs=lhs, A.rhs=rhs} =
      let
        (lhs', fs1) = findesc lhs
        (rhs', fs2) = findesc rhs
      in
       (exp{A.lhs=lhs', A.rhs=rhs'}, nub $ fs1 ++ fs2)
       
    findesc exp@(A.VarExp var) = undefined -- TODO
    
    findesc exp@A.RecordExp{A.fields=fields} =
      let
        (fields', fs) = foldr'
                        (\(sym, exp, pos) (fields, frees) ->
                          let
                            (exp', fs) = findesc exp
                          in
                           ((sym, exp', pos):fields, nub $ fs ++ frees))
                        ([], [])
                        fields
      in
       (exp{A.fields=fields'}, fs)
       
    findesc (A.SeqExp exps) =
      let
        (exps', fs) = foldr'
                      (\e (es, fs) ->
                        let
                          (e', fs') = findesc e
                        in
                         (e':es, nub $ fs' ++ fs))
                      ([], [])
                      exps
      in
       (A.SeqExp exps', fs)
                      
  in
   findesc
