import Prog1_5

-- maxargs 
maxargs :: Stm -> Int
maxargs stm = 
  case stm of
    CompoundStm stm1 stm2 -> max (maxargs stm1) (maxargs stm2)
    AssignStm _ e         -> maxargs_exp e
    PrintStm es           -> max (length es) (maxargs_es es)
  where
    maxargs_es :: [Exp] -> Int
    maxargs_es [] = 0
    maxargs_es (e:es) = max (maxargs_exp e) (maxargs_es es)
    maxargs_exp :: Exp -> Int
    maxargs_exp e =
      case e of
        EseqExp stm _ -> maxargs stm
        _             -> 0
          
main :: IO()  
main = putStrLn $ show $ maxargs prog


