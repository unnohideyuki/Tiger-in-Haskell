import Parser (parse)
import Lexer (alexScanTokens, prettyToken)
import Semant
import Env
import FindEscape

main = do
  s <- getContents
  let tokens = alexScanTokens s
  let ast = parse tokens
  let ast' = findEscape ast
  print ast'

