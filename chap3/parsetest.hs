import Parser (parse)
import Lexer (alexScanTokens)

main = do
  s <- getContents
  let tokens = alexScanTokens s
  print tokens
  print $ parse tokens
