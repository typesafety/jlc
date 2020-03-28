SRCDIR = src
BNFC_MAKEFILE_NAME = BNFC_GENERATED.make

.PHONY: clean

all: jlc


build: grammar
	cabal configure --ghc
	cabal build

jlc: clean build
	cabal install --installdir="./"

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf \
	--makefile=$(BNFC_MAKEFILE_NAME)
	cd $(SRCDIR) && make --makefile=$(BNFC_MAKEFILE_NAME) all

submissionA:
	tar -czf RENAMETHIS.tar.gz doc lib src jlc.cabal Makefile

TESTARCHIVE = partA-1.tar.gz
testA:
	tar -czf $(TESTARCHIVE) doc lib src jlc.cabal Makefile
	cd tester && python3 testing.py --archive ../$(TESTARCHIVE)

clean:
	-rm -r dist-newstyle
	-rm ./jlc
	-rm cabal.project.local*
	-rm -r $(SRCDIR)/Javalette
	-rm $(SRCDIR)/$(BNFC_MAKEFILE_NAME)
