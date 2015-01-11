tandorc:	tandorc.hs src/Lexer.hs src/Parser.hs
	ghc -Wall -isrc $<

src/Lexer.hs:	src/Lexer.x
	alex $^

src/Parser.hs:	src/Parser.y
	happy $^

clean:
	rm -f tandorc src/Lexer.hs src/Parser.hs


