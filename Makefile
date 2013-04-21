default: credit

credit: Block.hs Overlay.hs Prac1.hs Main.hs ANSIEscapes.hs
	ghc -lncurses --make Main -o credit

test: Test.hs
	ghc -lncurses --make Test -o test
