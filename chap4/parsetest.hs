import Parser (parse)
import Lexer (alexScanTokens, prettyToken)

main = do
  s <- getContents
  let tokens = alexScanTokens s
  print tokens
  print $ parse tokens
