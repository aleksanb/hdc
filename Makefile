configure:
	cabal configure

build: configure
	cabal build

clean:
	cabal clean
