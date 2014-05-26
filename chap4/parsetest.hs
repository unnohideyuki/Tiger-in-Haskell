import Parser (parse)
import Lexer (alexScanTokens)

main = do
  getContents >>= print . parse . alexScanTokens
