import Lexer

printTokens :: [Token] -> IO ()
printTokens [] = return ()
printTokens (t:ts) = do putStrLn $ prettyToken t
                        printTokens ts
main = do
  s <- getContents
  printTokens $  alexScanTokens s
