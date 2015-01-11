sources = src/Absyn.hs src/Canon.hs src/Color.hs src/Env.hs src/Flow.hs \
	  src/Graph.hs src/Liveness.hs src/Parser.hs src/Symbol.hs \
          src/Translate.hs src/Types.hs src/Assem.hs src/Codegen.hs \
          src/DalvikFrame.hs src/FindEscape.hs src/FrameBase.hs \
          src/Lexer.hs src/MakeGraph.hs src/Semant.hs src/Temp.hs \
          src/Tree.hs


tandorc:	tandorc.hs $(sources)
	ghc -Wall -isrc $<

src/Lexer.hs:	src/Lexer.x
	alex $^

src/Parser.hs:	src/Parser.y
	happy $^

clean:
	rm -f tandorc src/Lexer.hs src/Parser.hs


