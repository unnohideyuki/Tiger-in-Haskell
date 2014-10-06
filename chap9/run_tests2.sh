#! /bin/bash
rm src/*.o src/*.hi
cd src/
alex Lexer.x
happy Parser.y
ghc Lexer.hs
ghc Parser.hs
cd ..
ghc -Wall -isrc driver2.hs

for f in ../testcases/*.tig ../additonalcases/*.tig ;do 
    echo -n $f ": "; ./driver2 < $f; done > results2.log 2>&1

