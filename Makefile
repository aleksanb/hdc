configure:
	cabal configure

build: configure
	cabal build

clean:
	cabal clean

test:
	./dist/build/hdc/hdc < source_programs/tesla.d
