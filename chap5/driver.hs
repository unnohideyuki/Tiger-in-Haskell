import Parser (parse)
import Lexer (alexScanTokens, prettyToken)
import Semant
import Env

main = do
  s <- getContents
  let tokens = alexScanTokens s
  let absyn = parse tokens
  -- print tokens
  print absyn
  print $ transExp base_venv base_tenv absyn
