#! /bin/bash

cd src/
alex Lexer.x
happy Parser.y
cd ..
ghc -isrc driver.hs

for f in ../testcases/*.tig ../additonalcases/*.tig ;do 
    echo -n $f ": "; ./driver < $f; done > results.log 2>&1

