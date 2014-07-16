module Translate where

import qualified Temp
import qualified Frame hiding (formals)
import qualified DalvikFrame as Frame
import qualified Tree as T

data Level = Level { parent :: Level
                   , name :: Temp.Label
                   , formals :: [Bool]
                   , frame :: Frame.Frame
                   , uniq :: Int
                   }
           | Outermost
             deriving (Show)
                      
instance Eq Level where
  Level{uniq=u1} == Level{uniq=u2} = u1 == u2
  Outermost == Outermost = True
  _ == _ = False

data Access = Access { level :: Level, access :: Frame.Access }
              deriving (Eq, Show)

outermost = Outermost

newLevel :: Level -> Temp.Label -> [Bool] -> Temp.Temp -> (Level, Temp.Temp)
newLevel parent name formals temp = 
  let
    (frame, temp') = Frame.newFrame name formals temp
    (n, temp'') = Temp.newNum temp'
  in
   (Level{parent=parent ,name=name, formals=formals, frame=frame , uniq=n},
    temp'')

allocLocal :: Level -> Bool -> Temp.Temp -> (Access, Level, Temp.Temp)
allocLocal level@Level{frame=frame} escapes temp =
  let
    (access, frame', temp') = Frame.allocLocal frame escapes temp
    level' = level{frame=frame'}
  in
   (Access{level=level', access=access}, level', temp')

data Exp = Ex T.Exp
         | Nx T.Stm
         | Cx (Temp.Label -> Temp.Label -> T.Stm)

mkseq (stm1:stm2:[]) = T.SEQ stm1 stm2
mkseq [] = T.EXP $ T.CONST 0
mkseq (stm:stms) = T.SEQ stm $ mkseq stms

unEx :: Temp.Temp -> Exp -> (T.Exp, Temp.Temp)
unEx temp = 
  let
    unex (Ex e) = (e, temp)

    unex (Cx genstm) =
      let (r, temp') = Temp.newTemp temp
          (t, temp'') = Temp.newLabel temp'
          (f, temp3) = Temp.newLabel temp''

          e = T.ESEQ 
              (mkseq [T.MOVE (T.TEMP r) (T.CONST 1),
                    genstm t f,
                    T.LABEL f,
                    T.MOVE (T.TEMP r) (T.CONST 0),
                    T.LABEL t])
              (T.TEMP r)
      in
       (e, temp3)
       
    unex (Nx s) = (T.ESEQ s $ T.CONST 0, temp)
  in
   unex

unNx :: Temp.Temp -> Exp -> (T.Stm, Temp.Temp)
unNx temp =
  let
    unnx (Ex e) = (T.EXP e, temp)
    
    unnx (Cx genstm) =
      let
        (t, temp') = Temp.newLabel temp
        e = T.ESEQ (mkseq [genstm t t, T.LABEL t]) (T.CONST 0)
      in        
       (T.EXP e, temp')
       
    unnx (Nx s) = (s, temp)
  in
   unnx
       
unCx :: Exp -> (Temp.Label -> Temp.Label -> T.Stm)
unCx =
  let
    uncx (Ex (T.CONST 1)) = (\t _ -> T.JUMP (T.NAME t) [t])
    uncx (Ex (T.CONST 0)) = (\_ f -> T.JUMP (T.NAME f) [f])
    uncx (Ex e) = (\t f -> T.CJUMP T.NE e (T.CONST 0) t f)
    uncx (Cx genstm) = genstm
    uncx (Nx _) = undefined
  in
   uncx

nilExp :: Exp
nilExp = Ex $ T.CONST 0

intExp :: Int -> Exp
intExp i = Ex $ T.CONST i

binOp :: T.Binop -> Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
binOp op e1 e2 temp = 
  let
    (e1', temp') = unEx temp e1
    (e2', temp'') = unEx temp e2
  in
   (Ex $ T.BINOP op e1' e2', temp'')

plusOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
plusOp = binOp T.PLUS

minusOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
minusOp = binOp T.MINUS

timesOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
timesOp = binOp T.MUL

divideOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
divideOp = binOp T.DIV

relOp :: T.Relop -> Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
relOp op e1 e2 temp =
  let
    (e1', temp') = unEx temp e1
    (e2', temp'') = unEx temp e2
  in
   (Cx $ (\t f -> T.CJUMP op e1' e2' t f), temp'')

ltOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
ltOp = relOp T.LT
   
gtOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
gtOp = relOp T.GT

leOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
leOp = relOp T.LE

geOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
geOp = relOp T.GE

eqOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
eqOp = relOp T.EQ

neqOp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
neqOp = relOp T.NE

ifThenElse :: Exp -> Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
ifThenElse e1 e2 e3 temp = 
  let
    genstm = unCx e1
    (e2', temp') = unEx temp e2
    (e3', temp'') = unEx temp' e3
    (r, temp3) = Temp.newTemp temp''
    (t, temp4) = Temp.newLabel temp3
    (f, temp5) = Temp.newLabel temp4
    (j, temp6) = Temp.newLabel temp5
    
    e = T.ESEQ
        (mkseq [genstm t f,
                T.LABEL t,
                T.MOVE (T.TEMP r) e2',
                T.JUMP (T.NAME j) [j],
                T.LABEL f,
                T.MOVE (T.TEMP r) e3',
                T.LABEL j])
        (T.TEMP r)
  in
   -- TODO: optimize the case when e2 or e3 is not Ex
   (Ex e, temp6)

ifThen :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
ifThen e1 e2 temp = 
  let
    genstm = unCx e1
    (s, temp') = unNx temp e2
    (r, temp'') = Temp.newTemp temp'
    (t, temp3) = Temp.newLabel temp''
    (f, temp4) = Temp.newLabel temp3
    
    e = T.ESEQ
        (mkseq [genstm t f,
                T.LABEL t,
                s,
                T.LABEL f])
        (T.CONST 0)
  in
   (Ex e, temp4)
                
simpleVar :: Access -> Level -> Exp
simpleVar Access{level=lev_dec, access=acc} lev_use =
  let
    fpexp = follow_links lev_use lev_dec
        
    follow_links lev_use lev_dec =
      let
        follow' levu levd fpexp =
          if levu == levd then
            fpexp
          else
            let
              levu' = parent levu
              fpexp' = Frame.static_link fpexp
            in
             follow' levu' levd fpexp'
        
        curr_frame = frame lev_use
      in
       follow' lev_use lev_dec $ T.TEMP $ Frame.fp curr_frame
  in
   Ex $ Frame.exp acc fpexp

subscriptVar :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
subscriptVar var idx temp =
  let
    (v, temp') = unEx temp var
    (i, temp'') = unEx temp' idx
  in
   (Ex $ T.ARR v i, temp'')
   
fieldVar :: Exp -> Int -> Temp.Temp -> (Exp, Temp.Temp)
fieldVar var i temp =
  let (v, temp') = unEx temp var
  in (Ex $ T.RCD v i, temp')

recordExp :: [Exp] -> Temp.Temp -> (Exp, Temp.Temp)
recordExp cs temp = 
  let
    len = Ex $ T.CONST $ length cs
  in
   callExp (Temp.namedLabel "_initRecord") (len:cs) temp
   
arrayExp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
arrayExp size init temp =
  callExp (Temp.namedLabel "_initArray") [size, init] temp

seqExp :: [Exp] -> Temp.Temp -> (Exp, Temp.Temp)
seqExp [] temp = (Nx $ T.EXP $ T.CONST 0, temp)
seqExp es temp = 
  let
    (lst, temp') = unEx temp (last es) 
    (int, temp'') = foldr
                   (\e (es, temp) -> case unNx temp e of
                       (e', temp') -> (e':es, temp')
                   )
                   ([], temp)
                   (init es)
  in
   case length es of
     1 -> (Ex $ lst, temp')
     2 -> (Ex $ T.ESEQ (head int) lst, temp'')
     _ -> (Ex $ T.ESEQ (mkseq int) lst, temp'')
      
assignExp :: Exp -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
assignExp lhs rhs temp = 
  let
    (e1, temp') = unEx temp lhs
    (e2, temp'') = unEx temp' rhs
  in
   (Nx $ T.MOVE e1 e2, temp'')
   
whileExp :: Exp -> Exp -> Temp.Label -> Temp.Temp -> (Exp, Temp.Temp)
whileExp test body brkdest temp = 
  let
    genstm = unCx test
    (sbody, temp') = unNx temp body
    (l1, temp'') = Temp.newLabel temp'
    (l2, temp3) = Temp.newLabel temp''

    tree = mkseq[T.LABEL l1,
                 genstm l2 brkdest,
                 T.LABEL l2,
                 sbody,
                 T.JUMP (T.NAME l1) [l1],
                 T.LABEL brkdest]
  in
   (Nx tree, temp3)
   
breakExp :: Temp.Label -> Exp
breakExp brkdest = Nx $ T.JUMP (T.NAME brkdest) [brkdest]

letExp :: [Exp] -> Exp -> Temp.Temp -> (Exp, Temp.Temp)
letExp es ebody temp = 
  let
    (ss, temp') = foldr
                  (\e (ss, temp) ->
                    let
                      (s, temp') = unNx temp e
                    in
                     (s:ss, temp')
                  )
                  ([], temp)
                  es
    (e, temp'') = unEx temp' ebody
    tree = T.ESEQ (mkseq ss) e
  in
   (Ex tree, temp'')

callExp :: Temp.Label -> [Exp] -> Temp.Temp -> (Exp, Temp.Temp)
callExp f exprs temp = 
  let
    (temp', args) = foldr
                    (\expr (temp, es) ->
                      case unEx temp expr of
                        (e, temp') -> (temp', e:es)
                    )
                    (temp, [])
                    exprs
  in
   (Ex $ T.CALL (T.NAME f) args, temp')

stringExp :: String -> Temp.Temp -> (Exp, Frame.Frag, Temp.Temp)
stringExp s temp =
  let
    (label, temp') = Temp.newLabel temp
    frag = Frame.Str label s
    expr = Ex $ T.NAME label
  in
   (expr, frag, temp')
   
acc_formals :: Level -> [Access]
acc_formals level =
    fmap 
    (\a -> Access{level=level, access=a})
    (Frame.formals $ frame level)

   
fpExp :: Level -> Exp
fpExp level = 
   Ex $ T.TEMP $ Frame.fp $ frame level