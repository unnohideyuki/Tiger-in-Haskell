{

module Parser (parse) where
import Lexer

}

%name parse
%tokentype { Token }
%error { parseError }

%token
while   { While _ }
for     { For _ }
to      { To _ }
break   { Break _ }
let     { Let _ }
in      { In _ }
end     { End _ }
function { Function _ }
var     { Var _ }
type    { Type _ }
array   { Array _ }
if      { If _ }
then    { Then _ }
else    { Else _ }
do      { Do _ }
of      { Of _ }
nil     { Nil _ }
','     { Comma _ }
':'     { Colon _ }
';'     { Semicolon _ }
'('     { Lparen _ }
')'     { Rparen _ }
'['     { Lbrack _ }
']'     { Rbrack _ }
'{'     { Lbrace _ }
'}'     { Rbrace _ }
'.'     { Dot _ }
'+'     { Plus _ }
'-'     { Minus _ }
'*'     { Times _ }
'/'     { Divide _ }
'='     { Eq _ }
'<>'    { Neq _ }
'<'     { Lt _ }
'<='    { Le _ }
'>'     { Gt _ }
'>='    { Ge _ }
'&'     { And _ }
'|'     { Or _ }
':='    { Assign _ }
STRING  { Strliteral _ $$ }
INT     { Intliteral _ $$ }
ID      { Id _ $$}

%right    of
%nonassoc do
%nonassoc else
%nonassoc ':='
%left     '&' '|'
%nonassoc '=' '<>' '<' '<=' '>' '>=' 
%left     '+' '-'
%left     '*' '/'
%left     UMINUS

%%

program:        exp     {}

decs:           tydec decs   {}
 |              vardec decs  {}
 |              fundec decs  {}
 |              {- empty -}  {}

tydec:          type ID '=' ty { }

ty:             ID                      {}
 |              '{' tyfields '}'        {}
 |              array of ID             {}

tyfields:       ID ':' ID tyfs_tail     {}
 |              {- empty -}             {}

tyfs_tail:      ',' ID ':' ID tyfs_tail {}
 |              {- empty -}             {}

vardec:         var ID ':=' exp         {}
 |              var ID ':' ID ':=' exp  {}

fundec:         function ID '(' tyfields ')' '=' exp            {}
 |              function ID '(' tyfields ')' ':' ID '=' exp     {}

lvalue:         ID                      {}
 |              lval2                   {}

lval2:          ID '.' ID               {}
 |              lval2 '.' ID            {}
 |              ID '[' exp ']'          {}
 |              lval2 '[' exp ']'       {}

exp:            lvalue                                          {}
 |              nil                                             {}
 |              '(' expseq ')'                                  {}
 |              INT                                             {}
 |              STRING                                          {}
 |              '-' exp %prec UMINUS                            {}
 |              ID '(' args ')'                                 {}
 |              exp '+' exp                                     {}
 |              exp '-' exp                                     {}
 |              exp '*' exp                                     {}
 |              exp '/' exp                                     {}
 |              exp '=' exp                                     {}
 |              exp '<>' exp                                    {}
 |              exp '<' exp                                     {}
 |              exp '>' exp                                     {}
 |              exp '<=' exp                                    {}
 |              exp '>=' exp                                    {}
 |              exp '&' exp                                     {}
 |              exp '|' exp                                     {}
 |              ID '{' rcd '}'                                  {}
 |              ID '[' exp ']' of exp                           {}
 |              lvalue ':=' exp                                 {} 
 |              if exp then exp else exp                        {}
 |              if exp then exp %prec do                        {}
 |              while exp do exp                                {}      
 |              for ID ':=' exp to exp do exp                   {}
 |              break                                           {}
 |              let decs in expseq end                          {}


expseq:         exp expseq_tail         {}
 |              {- empty -}             {}

expseq_tail:    ';' exp expseq_tail     {}
 |              {- empty -}             {}

args:           exp args_tail           {}
 |              {- empty -}             {}

args_tail:      ',' exp args_tail       {}
 |              {- empty -}             {}

rcd:            ID '=' exp rcd_tail     {}
 |              {- empty -}             {}

rcd_tail:       ',' ID '=' exp rcd_tail {}
 |              {- empty -}             {}


{
parseError :: [Token] -> a
parseError [] = error "Parse Error at EOF"
parseError (x:xs) = error ("Parse Error at token " ++ prettyToken x)
}
