.PHONY:
configure:
	cabal configure

build: configure
	cabal build

.PHONY:
clean:
	cabal clean

.PHONY:
test:
	./test_runner.sh
