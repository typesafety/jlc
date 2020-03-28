SRCDIR = src
BNFC_MAKEFILE_NAME = BNFC_GENERATED.make

all: build install

build: grammar
	cabal configure --ghc
	cabal build

install: build
	cabal install --installdir="./"

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf \
	--makefile=$(BNFC_MAKEFILE_NAME)
	cd $(SRCDIR) && make --makefile=$(BNFC_MAKEFILE_NAME) all

test:
	@echo "test: Not yet implemented"

clean:
	rm -r dist-newstyle
	rm ./jlc
	rm cabal.project.local*
	rm -r $(SRCDIR)/Javalette
	rm $(SRCDIR)/$(BNFC_MAKEFILE_NAME)
