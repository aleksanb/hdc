.PHONY:
configure:
	cabal configure

.PHONY:
parser:
	happy src/Parser.y

.PHONY:
tokenizer:
	alex src/Tokenizer.x

build: tokenizer parser configure
	cabal build

.PHONY:
run:
	cd src && runhaskell Main.hs

.PHONY:
clean:
	cabal clean

.PHONY:
test:
	./test_runner.sh
