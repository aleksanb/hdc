.PHONY:
configure:
	cabal configure

.PHONY:
parser:
	cabal run happy src/Parser.y

.PHONY:
tokenizer:
	cabal run alex src/Tokenizer.x

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
