HSFLAGS=-w

all:
	ghc genKeys 	$(HSFLAGS)
	ghc encrypt 	$(HSFLAGS)
	ghc decrypt 	$(HSFLAGS)
	ghc notBombe	$(HSFLAGS)

test:
	ghc isPrime	$(HSFLAGS)

clean:
	-rm genKeys encrypt decrypt *.{o,hi}
