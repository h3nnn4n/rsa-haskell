HSFLAGS=-w

all:
	ghc genKeys 	$(HSFLAGS)
	ghc encrypt 	$(HSFLAGS)
	ghc decrypt 	$(HSFLAGS)
	ghc notBombe	$(HSFLAGS)

clean:
	-rm genKeys encrypt decrypt *.{o,hi}
