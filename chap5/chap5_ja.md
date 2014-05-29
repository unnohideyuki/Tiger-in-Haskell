Chapter 5 Semantic Analysis
===========================

この章では、Parse 結果として得られた抽象構文木に意味解析（型検査）を行う。

Lexer.x, Parser.y, Absyn.hs は、４章のものを必要に応じて修正して用いる。
あらたに増えたのは、Symbol.hs, Types.hs, Env.hs と Semant.hs である。

## ４章から引き続き用いるファイル
### Lexer.x

Lexer.x は変更なし

### Parser.y

Parser.y には２つの変更を施した:

+ A.RecordTy に Pos を渡すように変更
+ A.LetExp に渡す decs をマージしてから渡すようにした

前者は、Unique 値（これについては Type のところで後述）を Pos （その宣言のソース上での位置） 
から生成することにしたので、レコード型にも Pos をつけたというもの。

後者は、４章のメモで積み残した decs のマージである。
隣接する TypeDec や FunctionDec 同士は相互参照が許されるので、これらをマージしてから LetExp 
に渡すようにしてある。decs の順番はソース上における順番を保存している必要があることと、
Tiger 言語では VerDec で分断された宣言同士は相互参照できないことに注意。

### Absyn.hs

Absyn.hs も、ほとんど変わっていない

+ A.RecordTy に Pos を追加した
+ Pos の show 関数を書いて、表示を簡潔にした
+ Symbol を String から Symbol.Symbol のシノニムにした

## ５章で新たに作成したファイル

### Symbol.hs

自前で実装はせず、Data.Map をラップしただけ。

### Types.hs

教科書の PROGRAM 5.7 を Haskell で書き直すだけなのだが、Unique の扱いが異なる。

教科書では、Unique を unit ref 型としている。各 ref がユニークになることを利用しているとのことだが、
Haskell で同様のことをやるのは面倒そうだったので、ユニークな整数を生成して用いることにした。

どうやって、その整数を生成するかが問題になるのだが、
今回はお手軽に、その型が宣言されたソース上での位置から生成することにした。

また、RECORD, ARRAY の比較は unique 値のみで判定するようにした。Ty を単純に deriving (Eq)
によって比較可能にすると、全フィールドが比較されてしまうが、これではダメなケースがあったため。
(例： ../additionalcases/reccmp.tig )

これは、フィールドの型が再帰的な場合に Nothing を消しきれていないせい。消せてなくていいのかどうか、
まだよくわかっていない。

### Env.hs

素直に教科書にかかれていることを Haskell で書き直しただけ。

### Semant.hs

この Semant.hs を書くのが、この章のメイン。

    transExp :: A.Exp -> ExpTy

trans なんとかという名前は、後（７章）に、この関数は中間言語への変換関数に化けることを示しているようだ。
そのときには、ExpTy は中間形式と型の両方を含むようになるはずだが、いまは型だけが出力される。


TypeDec に対する transDec の実装は苦労した。最初にダミーの「ヘッダ」を環境に追加し、
それから transTy しなさいと教科書にあるのだが、それだけではうまくいかず、
さらに update (コーディング上は transTy の最後の引数を True にしたときに更新動作）するようにした。
都合 3 pass である。

それでも、Nothing がすべてはなくなっていない。Record Field に関しては自己参照的な型宣言が可能なので、
普通にはなくせないんじゃないの、という気もする。NAME 型については、
3rd pass 時にどんどん参照をたどり、循環参照も検出するようにしたが、Record field 
で同じことはできないはず。

ML 版では NAME の型が ref 変数への代入になっているのだが、
Haskell で書くにあたっては純粋関数的に書いているのが、Nothing の置き換え時の挙動に影響してそう。
…だが、よくわからない。

Cyclic Dependency の検出まわりもあやふや。transDec, transTy の両方に検出ロジックを書いてしまっている
（私が混乱している証拠？）のだが、前者に引っかかるみたい。(test16b)

## テスト

driver.hs と、
Tiger book のサイトにある [testcases](http://www.cs.princeton.edu/~appel/modern/testcases/)
を用いてテストした結果は以下の通り：

    ../testcases/test1.tig : ExpTy {ty = ARRAY INT 30025}
    ../testcases/test10.tig : driver: 2:1: type mismatch: expected UNIT, actual INT
    ../testcases/test11.tig : driver: 2:1: type mismatch: expected INT, actual STRING
    ../testcases/test12.tig : ExpTy {ty = UNIT}
    ../testcases/test13.tig : driver: 3:3: type mismatch: expected INT, actual STRING
    ../testcases/test14.tig : driver: 12:16: type mismatch: expected RECORD [("name",STRING),("id",INT)] 60024, actual ARRAY INT 50024
    ../testcases/test15.tig : driver: 3:1: type mismatch: expected UNIT, actual INT
    ../testcases/test16.tig : ExpTy {ty = STRING}
    ../testcases/test17.tig : ExpTy {ty = INT}
    ../testcases/test18.tig : ExpTy {ty = INT}
    ../testcases/test19.tig : ExpTy {ty = INT}
    ../testcases/test2.tig : ExpTy {ty = ARRAY INT 40025}
    ../testcases/test20.tig : ExpTy {ty = UNIT}
    ../testcases/test21.tig : driver: 8:32: : integer required.
    ../testcases/test22.tig : driver: 7:13: field not found: nam
    ../testcases/test23.tig : driver: 8:17: type mismatch: expected INT, actual STRING
    ../testcases/test24.tig : driver: 5:10: not an array
    ../testcases/test25.tig : driver: 5:10: not a record: INT
    ../testcases/test26.tig : driver: 3:3: : integer required.
    ../testcases/test27.tig : ExpTy {ty = INT}
    ../testcases/test28.tig : driver: 7:13: type mismatch: expected RECORD [("name",STRING),("id",INT)] 40025, actual RECORD [("name",STRING),("id",INT)] 50025
    ../testcases/test29.tig : driver: 7:13: type mismatch: expected ARRAY INT 40025, actual ARRAY INT 50025
    ../testcases/test3.tig : ExpTy {ty = RECORD [("name",STRING),("age",INT)] 30025}
    ../testcases/test30.tig : ExpTy {ty = INT}
    ../testcases/test31.tig : driver: 3:13: type mismatch: expected INT, actual STRING
    ../testcases/test32.tig : ExpTy {ty = INT}
    ../testcases/test33.tig : ExpTy {ty = INT}
    ../testcases/test34.tig : driver: 5:9: type mismatch: expected INT, actual STRING
    ../testcases/test35.tig : driver: 5:9: wrong number of arguments.
    ../testcases/test36.tig : driver: 5:9: wrong number of arguments.
    ../testcases/test37.tig : ExpTy {ty = INT}
    ../testcases/test38.tig : ExpTy {ty = INT}
    ../testcases/test39.tig : ExpTy {ty = INT}
    ../testcases/test4.tig : ExpTy {ty = INT}
    ../testcases/test40.tig : driver: 3:18: type mismatch: expected UNIT, actual INT
    ../testcases/test41.tig : ExpTy {ty = INT}
    ../testcases/test42.tig : ExpTy {ty = UNIT}
    ../testcases/test43.tig : driver: 6:11: : integer required.
    ../testcases/test44.tig : ExpTy {ty = UNIT}
    ../testcases/test45.tig : driver: 5:13: nil can be used only in the long form.
    ../testcases/test46.tig : ExpTy {ty = INT}
    ../testcases/test47.tig : ExpTy {ty = INT}
    ../testcases/test48.tig : ExpTy {ty = INT}
    ../testcases/test49.tig : driver: Parse Error at token nil at line 5, col 25
    ../testcases/test5.tig : ExpTy {ty = RECORD [("hd",INT),("tl",NAME "intlist" (Just (RECORD [("hd",INT),("tl",NAME "intlist" Nothing)] 40016)))] 40016}
    ../testcases/test6.tig : ExpTy {ty = UNIT}
    ../testcases/test7.tig : ExpTy {ty = INT}
    ../testcases/test8.tig : ExpTy {ty = INT}
    ../testcases/test9.tig : driver: 3:1: type mismatch: expected INT, actual STRING


これらの結果から、
型検査結果だけに注目する限りにおいては（内部的にはあやしい動きをしているかもしれないが）
一応大丈夫なんじゃないかと思う。

いくつか気になったもの：

Haskell の遅延評価のせいで、出力に関係ない部分が評価されていない。
test16, 19, 20, 32, 33, 37, 38 などがエラーしていないのは、そのため。また、
test23 はエラーしているが、 SeqExp の最後しか型検査されていないことがわかる。

test17, test48 のおかげで、Tiger 言語の使用を誤解していたことがわかった。
はじめ、let .. in 部分の宣言は、どれも相互参照できるように実装した（そのように merge_decs をつくった）
のだが、それは間違いだった。


また、遅延評価のせいでテストされていなかったものを補うために、いくつかのテストは修正したもの追加している：


    ../additonalcases/reccmp.tig : ExpTy {ty = INT}
    ../additonalcases/test16b.tig : driver: 4:1: cyclic dependency: a
    ../additonalcases/test17b.tig : driver: 4:12: type not defined (field): treelist
    ../additonalcases/test19b.tig : driver: 8:29: undefined variable: a
    ../additonalcases/test32b.tig : driver: 6:18: type mismatch: expected INT, actual STRING
    ../additonalcases/test33b.tig : driver: 3:17: record type not found: rectype
    ../additonalcases/test37b.tig : ExpTy {ty = STRING}
    ../additonalcases/test38b.tig : driver: 5:9: duplicated defintion: a
    ../additonalcases/test39b.tig : driver: 5:18: duplicated defintion: g
    ../additonalcases/test41b.tig : ExpTy {ty = STRING}
    ../additonalcases/test48b.tig : ExpTy {ty = INT}
    ../additonalcases/test5b.tig : ExpTy {ty = INT}

test5 の結果が気になっている。Nothing を消しきれないせいで、lis の型と lis.tl の型がどちらも intlist なのに、完全には一致していない。

lis の型：

    RECORD [("hd",INT),("tl",NAME "intlist" (Just (RECORD [("hd",INT),("tl",NAME "intlist" Nothing)] 40016)))] 40016

lis.ti の型:

    RECORD [("hd",INT),("tl",NAME "intlist" Nothing)] 40016

そのため、型の比較を完全一致かどうかで判定すると、誤って不一致にしてしまう。
そこで、T.Type の (==) 関数は unique 値のみを比較するようにした。
reccmp.tig は、この確認のために用意した。


## つみのこし

+ Eof トークンはいらない気がする
+ 文字列リテラル中のエスケープ文字の処理はまだだ
+ break と for/while の対応が正しいかどうかについては、なにもチェックしていない。７章できちんとやる必要があるはずなので、そこまで後回し


## Haskell 練習的なメモ

+ あまり使ったことのなかった foldl を、今回はよく使った。foldl が使える場合に、自前で再帰関数を書いてしまうのは良くないのかなと思い。
+ merge_decs (in Parser.y) のところ、もしかして foldr にすると上手くいくの？
+ p116 で述べられている transExp の書き方は、素敵だ。変数のスコープとカリー化をうまく利用している。もちろん、それにならって書いた
+ Debug.Trace というものを始めて使った。いっぱい使った。（最新のソースからは trace は消してある）

## 不安なところ

+ transTy はなんか怪しい気がする。header 登録、transTy、update の３フェーズにしてあるのだが…
+ 最後のフェーズはあれでいいのか？ なぜ update のときに cyclic dependency が検出されない？ (test16b)
+ Nothing がなくなっていない件。あとで問題になるのかな。
