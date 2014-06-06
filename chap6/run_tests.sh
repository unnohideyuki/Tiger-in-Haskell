#! /bin/bash

alex Lexer.x
happy Parser.y
ghc driver.hs

for f in ../testcases/*.tig ../additonalcases/*.tig ;do 
    echo -n $f ": "; ./driver < $f; done > results.log 2>&1

