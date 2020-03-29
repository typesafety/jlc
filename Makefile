SRCDIR = src

.PHONY: clean

all: jlc

build: grammar
	stack init --force

jlc: clean build
	stack build --local-bin-path="./" --copy-bins

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf

submissionA:
	tar -czf RENAMETHIS.tar.gz doc lib src jlc.cabal Makefile

TESTARCHIVE = partA-1.tar.gz
testA:
	tar -czf $(TESTARCHIVE) doc lib src jlc.cabal Makefile
	cd tester && python3 testing.py --archive ../$(TESTARCHIVE)

clean:
	-rm ./jlc
	-rm -r $(SRCDIR)/Javalette/
	-rm -r .stack-work/
	-rm stack.*
