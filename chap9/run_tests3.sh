#! /bin/bash
rm src/*.o src/*.hi
cd src/
alex Lexer.x
happy Parser.y
ghc Lexer.hs
ghc Parser.hs
cd ..
ghc -Wall -isrc tandorc.hs

mkdir -p out

for f in ../testcases/*.tig ../additonalcases/*.tig ;do 
    bname=`basename $f .tig`;
    ./tandorc < $f > out/${bname}.log 2>&1;
done    

