SRCDIR = src

.PHONY: clean prep

all: prep jlc

prep:
	stack install alex
	stack install happy

build: grammar
	stack init --force

jlc: clean build
	stack build --local-bin-path="./" --copy-bins

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf

submissionA:
	tar -czf RENAMETHIS.tar.gz doc lib src jlc.cabal Makefile

clean:
	-rm ./jlc
	-rm -r $(SRCDIR)/Javalette/
	-rm -r .stack-work/
	-rm stack.*
