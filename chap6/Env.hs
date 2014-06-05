module Env ( Access, Ty, EnvEntry(..)
           , base_tenv, base_venv
           ) where

import qualified Symbol as S
import qualified Types as T
import qualified Translate as TL
import qualified Temp

type Access = ()
type Ty = T.Ty

data EnvEntry = VarEntry { access :: TL.Access, ty :: Ty }
              | FunEntry { level :: TL.Level
                         , label :: Temp.Label
                         , formals :: [Ty], result :: Ty }
                deriving (Eq, Show)
                
fromList xs = foldl (\acc (id, v) -> S.insert acc id v) S.empty xs

base_tenv :: S.Table T.Ty
base_tenv = fromList [("int", T.INT), ("string", T.STRING)]

base_venv :: S.Table EnvEntry
base_venv = fromList xs
  where
    global_fun name formals result =
      (name, FunEntry { level = TL.outermost, label = Temp.namedLabel name
                      , formals = formals, result = result})
    xs = [ global_fun "print" [T.STRING] T.UNIT
         , global_fun "flush" [] T.UNIT
         , global_fun "getchar" [] T.STRING
         , global_fun "ord" [T.STRING] T.INT
         , global_fun "chr" [T.INT] T.STRING
         , global_fun "size" [T.STRING] T.INT
         , global_fun "substring" [T.STRING, T.INT, T.INT] T.INT
         , global_fun "concat" [T.STRING, T.STRING] T.STRING          
         , global_fun "not" [T.INT] T.INT
         , global_fun "exit" [T.INT] T.UNIT
         ]
         




