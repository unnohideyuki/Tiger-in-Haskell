#! /bin/bash
rm src/*.o src/*.hi
cd src/
alex Lexer.x
happy Parser.y
ghc Lexer.hs
ghc Parser.hs
cd ..
ghc -Wall -isrc driver.hs

for f in ../testcases/*.tig ../additonalcases/*.tig ;do 
    echo -n $f ": "; ./driver < $f; done > results.log 2>&1

