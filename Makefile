default: credit

credit: ANSIEscapes.hs Display.hs Keys.hs TextBox.hs FOUL.hs TextEditor.hs Display.hs FoulParser.hs
	ghc -lncurses --make TextEditor -o credit

