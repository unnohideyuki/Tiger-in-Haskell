module Env ( Access, Ty, EnvEntry(..)
           , base_tenv, base_venv
           ) where

import qualified Symbol as S
import qualified Types as T

type Access = ()
type Ty = T.Ty

data EnvEntry = VarEntry { ty :: Ty }
              | FunEntry { formals :: [Ty], result :: Ty }
                deriving (Eq, Show)
                
fromList [] = S.empty
fromList ((s, e):xs) = S.insert (fromList xs) s e

base_tenv :: S.Table T.Ty
base_tenv = fromList [("int", T.INT), ("string", T.STRING)]

base_venv :: S.Table EnvEntry
base_venv = fromList xs
  where
    xs = [("print", FunEntry [T.STRING] T.UNIT)
         ,("flush", FunEntry [] T.UNIT)
         ,("getchar", FunEntry [] T.STRING)
         ,("ord", FunEntry [T.STRING] T.INT)
         ,("chr", FunEntry [T.INT] T.STRING)
         ,("size", FunEntry [T.STRING] T.INT)
         ,("substring", FunEntry [T.STRING, T.INT, T.INT] T.INT)
         ,("concat", FunEntry [T.STRING, T.STRING] T.STRING)             
         ,("not", FunEntry [T.INT] T.INT)
         ,("exit", FunEntry [T.INT] T.UNIT)
         ]
         




