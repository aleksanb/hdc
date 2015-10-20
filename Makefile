all: install

.PHONY:
install:
	cabal install

.PHONY:
build:
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
