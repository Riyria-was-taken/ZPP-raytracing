make:
	stack ghc src/Main.hs -- -isrc -O2 -threaded -rtsopts -o raytracing 

install:
	stack install mersenne-random-pure64
	stack install parallel

.PHONY: clean

clean:
	rm -r raytracing src/*.hi src/*.o
