import Parser (parse)
import Lexer (alexScanTokens, prettyToken)
import Semant
import Env
import FindEscape

main = do
  s <- getContents
  let tokens = alexScanTokens s
  let ast = parse tokens
  let (ast', _, _) = findEscape ast
  print $ transProg base_venv base_tenv ast'
  
