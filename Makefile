all:
	alex src/Tokenizer.x
	happy src/Parser.y
	(cd src && ghci Main.hs)
