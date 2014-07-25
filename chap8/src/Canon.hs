module Canon where

import qualified Tree as T
import qualified Temp

linearize :: T.Stm -> [T.Stm]
linearize stm0 =
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
      let (stms, e) = do_exp a
          (stms', el, temp') = reorder rest temp
      in if commute (stms', e)
         then (stms % stms', e::el, temp')
         else let (t, temp'') = Temp.newTemp temp'
              in (stms % (T.MOVE (T.TEMP t) e) % stms', T.TEMP t:el, temp'')
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
    
    -- TODO: ARR and RCD are also can be a l-value
    do_stm (T.MOVE (T.MEM e) b) temp =
      reorder_stm [e, b] (\[e', b'] -> T.MOVE e' b') temp
                            
    do_stm (T.MOVE (T.ESEQ s e) b) temp = do_stm (T.SEQ s (T.MOVE e b)) temp
    
    do_stm (T.EXP (T.CALL e el)) temp =
      reorder_stm (e:el) (\(e':el') -> T.EXP $ T.CALL e' el') temp
      
    do_stm (T.EXP e) temp = reorder_stm [e] (\[e'] -> T.EXP e') temp
    
    do_stm s temp = reorder_stm [] (\_ -> s) temp
    
    do_exp = undefined
  in
   undefined
