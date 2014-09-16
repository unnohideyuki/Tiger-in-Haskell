import Parser (parse)
import Lexer (alexScanTokens, prettyToken)
import Semant
import Env
import FindEscape
import qualified Canon as C
import DalvikFrame as Frame

main = do
  s <- getContents
  let tokens = alexScanTokens s
  let ast = parse tokens
  let (ast', _, _) = findEscape ast
  let (ty, frgs, t) = transProg base_venv base_tenv ast'
  let (stm:stms) = fmap Frame.get_body frgs
  -- TODO: not only stm but (stm:stms) should be linearized.
  let (stms1, t1) = C.linearize stm t
  let (stms2, t2, lab) = C.basicBlocks stms1 t1
  let stms3 = C.traceSchedule stms2 lab t2
  putStrLn "-- frgs --"    
  print frgs
  putStrLn "-- linearized --"    
  print stms1
  putStrLn "-- basic blocks --"    
  print (stms2, lab)
  putStrLn "-- trace scheduled --"    
  print stms3
  
