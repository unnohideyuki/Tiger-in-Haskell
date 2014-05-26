Chapter 4 Abstract Syntax
=========================

この章では、3 章までで作った Parser に Syntactic Actions 
を追加していくことで抽象構文木をつくる。

## Absyn.hs

Figure 4.8 にある Absyn を比較的素直に Haskell で書いたつもりだが、いくつかひっかかった。

Figure 4.8 では、たとえば ForExp にも VarDec にも escape という名のフィールドがあるが、
これをそのまま Haskell で書くと Multiple Definition エラーになる。
Haskell の場合は、フィールド名がそのままフィールドにアクセスするための関数名にもなるためだと思う。


## Parser.x

### 徐々に action を書いていくときの注意

3 章でつくった Parser.y では Syntactic Actions はすべて空になっていたので、
少しずつ埋めていくことになるのだが、型推論のために少し考慮が必要。空欄を残したまま一部を埋めていくと、
型チェックでエラーになるので、ダミーのアクション ({ undefined } など) で全て埋めてから、
部分的に書き換えていくといいだろう。

### %token の書き換え

これまでに作った Lexer.x, Parser.y は、そのまま流用できると考えていたが、変更が必要となった。

chap3/Parser.y における %token 部は、位置情報を捨てるようなパタンマッチを書いてあったが、
4 章の抽象構文木は位置情報を保持するので、捨てないようにしなければならない。

このために、Token データ型のうち、Strliterl, Intliteral, Id の定義を変更し、


         | Strliteral AlexPosn String
         | Intliteral AlexPosn Integer
         | Id AlexPosn String

から、

         | Strliteral (AlexPosn, String)
         | Intliteral (AlexPosn, Integer)
         | Id (AlexPosn, String)

にした。(位置, 内容) をペアにすることで、一つの変数に束縛できるようにしている。

## つみのこし

+ Eof トークンはいらない気がする
+ decs で、複数の FunctionDec や TypeDec はまとめないといけないはず