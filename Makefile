# Commands:

build: 
	ghc --make -O -o checkers Main.hs

prof:
	ghc --make -prof -o checkers Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f checkers
	rm -f *.hi
	rm -f *.o