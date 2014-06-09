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
                      
    findesc exp@A.AssignExp{A.vvar=var, A.exp=e} =
      let
        (var', fs1) = findescv var
        (e', fs2) = findesc e
      in
       (exp{A.vvar=var', A.exp=e'}, nub $ fs1 ++ fs2)
       
    findesc exp@A.IfExp{A.test=test, A.thene=texp, A.elsee=eexp} =
      let
        (test', fs1) = findesc test
        (texp', fs2) = findesc texp
        (eexp', fs3) = 
          case eexp of
            Just e -> let (e', fs3') = findesc e
                      in
                       (Just e', fs3')
            Nothing -> (Nothing, [])
      in
       (exp{A.test=test', A.thene=texp', A.elsee=eexp'},
        nub $ fs1 ++ fs2 ++ fs3)


    findesc exp@A.WhileExp{A.test=test, A.body=body} =
      let
        (test', fs1) = findesc test
        (body', fs2) = findesc body
      in
       (exp{A.test=test', A.body=body'}, nub $ fs1 ++ fs2)

    findesc exp@(A.BreakExp _) = (exp, [])

    findesc exp@A.LetExp{A.decs=decs, A.body=body} =
      let
        fesc_decs dec (decs, names, fs) = 
          let
            (dec', names', fs') = findescd dec
          in
           (dec':decs, nub $ names' ++ names, nub $ fs' ++ fs)
           
        (decs', names, fs1) = foldr' fesc_decs ([], [], []) decs
        
        (body', fs2) = findesc body
        
        fs' = [s | s <- (nub $ fs1 ++ fs2), notElem s names]
      in
       (exp{A.decs=decs', A.body=body'}, fs')

    findesc exp@A.ArrayExp{A.size=size, A.init=init} =
      let
        (size', fs1) = findesc size
        (init', fs2) = findesc init
      in
       (exp{A.size=size', A.init=init'}, nub $ fs1 ++ fs2)

    findesc exp@A.ForExp {A.svar=svar, A.lo=lo, A.hi=hi, A.body=body} =
      let
        (lo', fs1) = findesc lo
        (hi', fs2) = findesc hi
        (body', fs3) = findesc body
        fs3' = [s | s<-fs3, s /= svar]
      in
       (exp{A.lo=lo', A.hi=hi', A.body=body'}, nub $ fs1 ++ fs2 ++ fs3')
    
    findesc exp@A.CallExp{A.func=func, A.args=args} = 
      let
        (args', fs') = foldr'
                       (\arg (args, fs) ->
                         let
                           (arg', fs') = findesc arg
                         in
                          (arg':args, nub $ fs' ++ fs))
                       ([], [])
                       args
      in
       (exp{A.args=args'}, nub $ func:fs')


    findescv :: A.Var -> (A.Var, [S.Symbol])
    
    findescv v@(A.SimpleVar sym _) = (v, [sym])
    
    findescv (A.FieldVar var sym pos) =
      let
        (var', fs) = findescv var
      in
       (A.FieldVar var' sym pos, fs)
       
    findescv (A.SubscriptVar var exp pos) =
      let
        (var', fs1) = findescv var
        (exp', fs2) = findesc exp
      in
       (A.SubscriptVar var' exp' pos, nub $ fs1 ++ fs2)
    
    findescd :: A.Dec -> (A.Dec, [S.Symbol], [S.Symbol])
    findescd = undefined

  in
   findesc

