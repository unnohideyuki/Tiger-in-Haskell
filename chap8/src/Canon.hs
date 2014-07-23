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
              in (stms % (T.MOVE (T.TEMP t) e) % stms', T.TEMP t:el)
    reorder [] temp = (nop, [], temp)
  in
   undefined
