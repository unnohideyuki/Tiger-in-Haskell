{

module Lexer where

}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
@id = $alpha ($alpha | $digit | \_)*

$u = [ . \n] -- any character

tokens :-

$white+                 ;
"/*" ([$u # \*] | \* [$u # \/])* ("*")+ "/" ;

while                   {(\p s -> While p)}
for                     {(\p s -> For p)}
to                      {(\p s -> To p)}
break                   {(\p s -> Break p)}
let                     {(\p s -> Let p)}
in                      {(\p s -> In p)}
end                     {(\p s -> End p)}
function                {(\p s -> Function p)}
var                     {(\p s -> Var p)}
type                    {(\p s -> Type p)}
array                   {(\p s -> Array p)}
if                      {(\p s -> If p)}
then                    {(\p s -> Then p)}
else                    {(\p s -> Else p)}
do                      {(\p s -> Do p)}
of                      {(\p s -> Of p)}
nil                     {(\p s -> Nil p)}

","                     {(\p s -> Comma p)}
":"                     {(\p s -> Colon p)}
";"                     {(\p s -> Semicolon p)}
"("                     {(\p s -> Lparen p)}
")"                     {(\p s -> Rparen p)}
"["                     {(\p s -> Lbrack p)}
"]"                     {(\p s -> Rbrack p)}
"{"                     {(\p s -> Lbrace p)}
"}"                     {(\p s -> Rbrace p)}
"."                     {(\p s -> Dot p)}
"+"                     {(\p s -> Plus p)}
"-"                     {(\p s -> Minus p)}
"*"                     {(\p s -> Times p)}
"/"                     {(\p s -> Divide p)}
"="                     {(\p s -> Eq p)}
"<>"                    {(\p s -> Neq p)}
"<"                     {(\p s -> Lt p)}
"<="                    {(\p s -> Le p)}
">"                     {(\p s -> Gt p)}
">="                    {(\p s -> Ge p)}
"&"                     {(\p s -> And p)}
"|"                     {(\p s -> Or p)}
":="                    {(\p s -> Assign p)}

\" \"                   {(\p s -> Strliteral (p, unquot s))}
\" ([^\"]|\\ \")* \"    {(\p s -> Strliteral (p, unquot s))}

$digit+                 {(\p s -> Intliteral (p, (read s :: Integer)))}

@id                     {(\p s -> Id (p, s))}

-- todo: how to match EOF?

{
data Token =
     -- reserved words
       While AlexPosn
     | For AlexPosn
     | To AlexPosn
     | Break AlexPosn
     | Let AlexPosn
     | In AlexPosn
     | End AlexPosn
     | Function AlexPosn
     | Var AlexPosn
     | Type AlexPosn
     | Array AlexPosn
     | If AlexPosn
     | Then AlexPosn
     | Else AlexPosn
     | Do AlexPosn
     | Of AlexPosn
     | Nil AlexPosn
     -- punctuation symbols
     | Comma AlexPosn
     | Colon AlexPosn
     | Semicolon AlexPosn
     | Lparen AlexPosn
     | Rparen AlexPosn
     | Lbrack AlexPosn
     | Rbrack AlexPosn
     | Lbrace AlexPosn
     | Rbrace AlexPosn
     | Dot AlexPosn
     | Plus AlexPosn
     | Minus AlexPosn
     | Times AlexPosn
     | Divide AlexPosn
     | Eq AlexPosn
     | Neq AlexPosn
     | Lt AlexPosn
     | Le AlexPosn
     | Gt AlexPosn
     | Ge AlexPosn
     | And AlexPosn
     | Or AlexPosn
     | Assign AlexPosn
     --
     | Strliteral (AlexPosn, String)
     | Intliteral (AlexPosn, Integer)
     | Id (AlexPosn, String)
     | Eof AlexPosn
       deriving (Eq, Show)

prettyAlexPosn (AlexPn _ line col) = "line " ++ show line ++ ", col " ++ show col

prettyToken :: Token -> String
prettyToken c = tk ++ " at " ++ prettyAlexPosn pos where
  (tk, pos) = case c of
     --
     While p -> ("while", p)
     For p -> ("for", p)
     To p -> ("to", p)
     Break p -> ("break", p)
     Let p -> ("let", p)
     In p -> ("in", p)
     End p -> ("end", p)
     Function p -> ("function", p)
     Var p -> ("var", p)
     Type p -> ("type", p)
     Array p -> ("array", p)
     If p -> ("if", p)
     Then p -> ("then", p)
     Else p -> ("else", p)
     Do p -> ("do", p)
     Of p -> ("of", p)
     Nil p -> ("nil", p)
     --
     Comma p -> (",", p)
     Colon p -> (":", p)
     Semicolon p -> (";", p)
     Lparen p -> ("(", p)
     Rparen p -> (")", p)
     Lbrack p -> ("[", p)
     Rbrack p -> ("]", p)
     Lbrace p -> ("{", p)
     Rbrace p -> ("}", p)
     Dot p -> (".", p)
     Plus p -> ("+", p)
     Minus p -> ("-", p)
     Times p -> ("*", p)
     Divide p -> ("/", p)
     Eq p -> ("=", p)
     Neq p -> ("<>", p)
     Lt p -> ("<", p)
     Le p -> ("<=", p)
     Gt p -> (">", p)
     Ge p -> (">=", p)
     And p -> ("&", p)
     Or p -> ("|", p)
     Assign p -> (":=", p)
     --
     Strliteral (p, s) -> (show s, p)
     Intliteral (p, i) -> (show i, p)
     Id (p, s) -> ("id:" ++ s, p)
     Eof p -> ("EOF", p)

unquot (x:xs) = init xs
}
