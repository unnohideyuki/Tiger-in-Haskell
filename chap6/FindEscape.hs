module FindEscape where

import Data.List
import Data.Foldable (foldr')

import qualified Absyn as A
import qualified Symbol as S

{- findEscape returns a pair of following:
    - Appropriately escape flaged AST
    - List of free variables
-}


findEscape :: A.Exp -> (A.Exp, [S.Symbol], [S.Symbol])

findEscape =
  let
    findesc exp@A.NilExp = (exp, [], [])
    
    findesc exp@(A.IntExp _ _) = (exp, [], [])
    
    findesc exp@(A.StringExp _ _) = (exp, [], [])
    
    findesc exp@A.OpExp {A.lhs=lhs, A.rhs=rhs} =
      let
        (lhs', fs1, fs2) = findesc lhs
        (rhs', fs1', fs2') = findesc rhs
      in
       (exp{A.lhs=lhs', A.rhs=rhs'}, nub$fs1++fs1', nub$fs2++fs2')
       
    findesc exp@(A.VarExp var) = 
      let
        (var', fs1, fs2) = findescv var
      in
       (A.VarExp var', fs1, fs2)
    
    findesc exp@A.RecordExp{A.fields=fields} =
      let
        (fields', fs1, fs2) = foldr'
                              (\(sym, exp, pos) (fields, fs1, fs2) ->
                                let
                                  (exp', fs1', fs2') = findesc exp
                                in
                                 ((sym, exp', pos):fields, 
                                  nub $ fs1'++ fs1, nub $ fs2' ++ fs2))
                              ([], [], [])
                              fields
      in
       (exp{A.fields=fields'}, fs1, fs2)
       
    findesc (A.SeqExp exps) =
      let
        (exps', fs1, fs2) = foldr'
                            (\e (es, fs1, fs2) ->
                              let
                                (e', fs1', fs2') = findesc e
                              in
                               (e':es, nub$fs1'++fs1, nub$fs2'++fs2))
                            ([], [], [])
                            exps
      in
       (A.SeqExp exps', fs1, fs2)
                      
    findesc exp@A.AssignExp{A.vvar=var, A.exp=e} =
      let
        (var', fs1, fs2) = findescv var
        (e', fs1', fs2') = findesc e
      in
       (exp{A.vvar=var', A.exp=e'}, nub$fs1'++fs1, nub$fs2'++fs2)
       
    findesc exp@A.IfExp{A.test=test, A.thene=texp, A.elsee=eexp} =
      let
        (test', fs1, fs2) = findesc test
        (texp', fs1', fs2') = findesc texp
        (eexp', fs1'', fs2'') = 
          case eexp of
            Just e -> let (e', fs1, fs2) = findesc e
                      in
                       (Just e', fs1, fs2)
            Nothing -> (Nothing, [], [])
      in
       (exp{A.test=test', A.thene=texp', A.elsee=eexp'},
        nub $ fs1 ++ fs1'++ fs1'',
        nub $ fs2 ++ fs2'++ fs2'')

    findesc exp@A.WhileExp{A.test=test, A.body=body} =
      let
        (test', fs1, fs2) = findesc test
        (body', fs1', fs2') = findesc body
      in
       (exp{A.test=test', A.body=body'}, nub$fs1++fs1',nub$fs2++fs2')

    findesc exp@(A.BreakExp _) = (exp, [], [])

    findesc exp@A.LetExp{A.decs=decs, A.body=body} =
      let
        fesc_decs dec (decs, names, fs1, fs2) = 
          let
            (dec', names', fs1', fs2') = findescd dec fs1 fs2
          in
           (dec':decs, nub$names'++names, nub$fs1'++fs1, nub$fs2'++fs2)
           
        (body', fs1b, fs2b) = findesc body
        (decs', names, fs1, fs2) = foldr' fesc_decs ([], [], fs1b, fs2b) decs
      in
       (exp{A.decs=decs', A.body=body'}, fs1, fs2)

    findesc exp@A.ArrayExp{A.size=size, A.init=init} =
      let
        (size', fs1, fs2) = findesc size
        (init', fs1', fs2') = findesc init
      in
       (exp{A.size=size', A.init=init'}, nub$fs1++fs1', nub$fs2++fs2')

    findesc exp@A.ForExp {A.svar=svar, A.lo=lo, A.hi=hi, A.body=body} =
      let
        (lo', fs1, fs2) = findesc lo
        (hi', fs1', fs2') = findesc hi
        (body', fs1'', fs2'') = findesc body
        
        fs13 = [s | s<-fs1'', s /= svar]
        fs23 = [s | s<-fs2'', s /= svar]
        
        esc = or [True | s<-fs2'', s == svar]
      in
       (exp{A.lo=lo', A.hi=hi', A.body=body', A.escape=esc}, 
        nub $ fs1 ++ fs1' ++ fs13, nub $ fs2 ++ fs2' ++ fs23)
    
    findesc exp@A.CallExp{A.func=func, A.args=args} = 
      let
        (args', fs1', fs2') = foldr'
                              (\arg (args, fs1, fs2) ->
                                let
                                  (arg', fs1', fs2') = findesc arg
                                in
                                 (arg':args, nub$fs1'++fs1, nub$fs2'++fs2))
                              ([], [], [])
                              args
      in
       (exp{A.args=args'}, nub $func:fs1', fs2')


    findescv :: A.Var -> (A.Var, [S.Symbol], [S.Symbol])
    
    findescv v@(A.SimpleVar sym _) = (v, [sym], [])
    
    findescv (A.FieldVar var sym pos) =
      let
        (var', fs1, fs2) = findescv var
      in
       (A.FieldVar var' sym pos, fs1, fs2)
       
    findescv (A.SubscriptVar var exp pos) =
      let
        (var', fs1, fs2) = findescv var
        (exp', fs1', fs2') = findesc exp
      in
       (A.SubscriptVar var' exp' pos, nub$fs1++fs1', nub$fs2++fs2')
    
    findescd :: A.Dec -> [S.Symbol] -> [S.Symbol]
                -> (A.Dec, [S.Symbol], [S.Symbol], [S.Symbol])
    findescd = undefined

  in
   findesc

