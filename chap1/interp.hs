import qualified Data.Map as Map
import Prog1_5

type Table = Map.Map String Int

interpStm :: Stm -> Table -> IO Table
interpStm stm table = 
  case stm of
    CompoundStm stm1 stm2 -> do table'  <- interpStm stm1 table
                                table'' <- interpStm stm2 table'
                                return table''
    AssignStm id exp -> do (v, table') <- interpExp exp table
                           return $ Map.insert id v table'
    PrintStm exps -> do print_exps exps table
  where
    print_exps [] table = do putStrLn ""
                             return table
    print_exps (e:es) table = do (v, table') <- interpExp e table
                                 putStr $ show v
                                 putStr " "
                                 print_exps es table'

interpExp :: Exp -> Table -> IO (Int, Table)
interpExp exp table =
  case exp of
    IdExp id -> 
      case Map.lookup id table of
        Just v  -> return (v, table)
        Nothing -> return (undefined, table)
    NumExp v -> return (v, table)
    OpExp op exp1 exp2 -> do (v1, table')  <- interpExp exp1 table
                             (v2, table'') <- interpExp exp2 table'
                             return ((interpOp op v1 v2), table'')
    EseqExp stm exp -> do table' <- interpStm stm table
                          interpExp exp table'
  where
    interpOp Plus = (+)
    interpOp Minus = (-)
    interpOp Times = (*)
    interpOp Div = div
    
interp :: Stm -> IO()
interp stm = do table <- interpStm stm $ Map.empty
                return ()

main :: IO()
main = interp prog
