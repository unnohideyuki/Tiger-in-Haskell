module Canon where

import qualified Tree as T
import qualified Temp
import qualified Symbol

linearize :: T.Stm -> Temp.Temp -> ([T.Stm], Temp.Temp)
linearize stm0 temp0 =
  let
    infixl 0 %
    (T.EXP (T.CONST _)) % x = x
    x % (T.EXP (T.CONST _)) = x
    x % y = T.SEQ x y
    
    commute (T.EXP (T.CONST _), _) = True
    commute (_, T.NAME _) = True
    commute (_, T.CONST _) = True
    commute _ = False
    
    nop = T.EXP $ T.CONST 0
    
    reorder (e@(T.CALL _ _):rest) temp =
      let (t, temp') = Temp.newTemp temp
      in reorder (T.ESEQ (T.MOVE (T.TEMP t) e) (T.TEMP t) : rest) temp'
    reorder (a:rest) temp =
      let (stms, e, temp') = do_exp a temp
          (stms', el, temp'') = reorder rest temp'
      in if commute (stms', e)
         then (stms % stms', e:el, temp'')
         else let (t, temp3) = Temp.newTemp temp''
              in (stms % (T.MOVE (T.TEMP t) e) % stms', T.TEMP t:el, temp3)
    reorder [] temp = (nop, [], temp)
    
    reorder_exp el build temp =
      let
        (stms, el', temp') = reorder el temp
      in
       (stms, build el', temp')
       
    reorder_stm el build temp =
      let
        (stms, el', temp') = reorder el temp
      in
       (stms % build el', temp')
       
    do_stm (T.SEQ a b) temp = 
      let 
        (s1, t1) = do_stm a temp 
        (s2, t2) = do_stm b t1
      in
       (s1 % s2, t2)
       
    do_stm (T.JUMP e labs) temp = reorder_stm [e] (\[e'] -> T.JUMP e' labs) temp
  
    do_stm (T.CJUMP p a b t f) temp =
      reorder_stm [a, b] (\[a', b'] -> T.CJUMP p a' b' t f) temp
    
    do_stm (T.MOVE (T.TEMP t) (T.CALL e el)) temp =
      reorder_stm (e:el) (\(e':el') -> T.MOVE (T.TEMP t) (T.CALL e' el')) temp

    do_stm (T.MOVE (T.TEMP t) b) temp =
      reorder_stm [b] (\[b'] -> T.MOVE (T.TEMP t) b') temp
    
    do_stm (T.MOVE (T.MEM e) b) temp =
      reorder_stm [e, b] (\[e', b'] -> T.MOVE (T.MEM e') b') temp
                            
    do_stm (T.MOVE (T.ARR v i) b) temp =
      reorder_stm [v, i, b] (\[v', i', b'] -> T.MOVE (T.ARR v' i') b') temp
      
    do_stm (T.MOVE (T.RCD e n) b) temp =
      reorder_stm [e, b] (\[e', b'] -> T.MOVE (T.RCD e' n) b') temp
    
    do_stm (T.MOVE (T.ESEQ s e) b) temp = do_stm (T.SEQ s (T.MOVE e b)) temp
    
    do_stm (T.EXP (T.CALL e el)) temp =
      reorder_stm (e:el) (\(e':el') -> T.EXP $ T.CALL e' el') temp
      
    do_stm (T.EXP e) temp = reorder_stm [e] (\[e'] -> T.EXP e') temp
    
    do_stm s temp = reorder_stm [] (\_ -> s) temp
    
    do_exp (T.BINOP p a b) temp =
      reorder_exp [a, b] (\[a', b'] -> T.BINOP p a' b') temp
                            
    do_exp (T.MEM a) temp =
      reorder_exp [a] (\[a'] -> T.MEM a') temp
      
    do_exp (T.ARR v i) temp =
      reorder_exp [v, i] (\[v', i'] -> T.ARR v' i') temp
      
    do_exp (T.RCD e n) temp =
      reorder_exp [e] (\[e'] -> T.RCD e' n) temp
      
    do_exp (T.ESEQ s e) temp =
      let
        (stms, temp') = do_stm s temp
        (stms', e', temp'') = do_exp e temp'
      in
       (stms % stms', e', temp'')
                         
    do_exp (T.CALL e el) temp =
      reorder_exp (e:el) (\(e':el') -> T.CALL e' el') temp
      
    do_exp e temp = reorder_exp [] (\_ -> e) temp
    
    {- linear gets rid of the top-level SEQ's, producing a list -}
    linear (T.SEQ a b) l = linear a (linear b l)
    linear s l = s:l
    
    (ss, tret) = do_stm stm0 temp0
  in {- body of linearize -}
   (linear ss [], tret)
   
type Block = [T.Stm]

basicBlocks :: [T.Stm] -> Temp.Temp -> ([Block], Temp.Temp, Temp.Label)
basicBlocks stms0 temp0 =
  let
    (done, temp1) = Temp.newLabel temp0
    blocks (hd@(T.LABEL _):tl) blist temp = 
      let
        next (s@(T.JUMP _ _):rest) thisblock t = endblock rest (s:thisblock) t
        next (s@(T.CJUMP _ _ _ _ _):rest) thisblock t =
          endblock rest (s:thisblock) t
        next stms@(T.LABEL lab:_) thisblock t =
          next ((T.JUMP (T.NAME lab) [lab]):stms) thisblock t
        next (s:rest) thisblock t = next rest (s:thisblock) t
        next [] thisblock t =
          next [T.JUMP (T.NAME done) [done]] thisblock t
        
        endblock stms thisblock t = 
          blocks stms (reverse thisblock:blist) t
      in
       next tl [hd] temp
    
    blocks [] blist temp = (reverse blist, temp)
    
    blocks stms blist temp = 
      let
        (label, temp') = Temp.newLabel temp
      in
       blocks (T.LABEL label:stms) blist temp'
    
    (bs, tret) = blocks stms0 [] temp1
  in
   (bs, tret, done)
 
enterblock :: [T.Stm] -> Symbol.Table [T.Stm] -> Symbol.Table [T.Stm]
enterblock b@(T.LABEL s:_) table = Symbol.insert table s b
enterblock _ table = table
