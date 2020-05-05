SRCDIR = src

.PHONY: clean prep

all: prep jlc

prep:
	stack install alex
	stack install happy

jlc: clean
	stack init --force
	stack build --local-bin-path="./" --copy-bins

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf

submissionA:
	tar -czf partA-999.tar.gz doc lib src jlc.cabal Makefile

submissionB:
	tar -czf partB-REPLACETHIS.tar.gz doc lib src jlc.cabal Makefile

testB:
	tar -czf tester/partB-999.tar.gz doc lib src jlc.cabal Makefile
	cd tester && python3 testing.py partB-999.tar.gz \
		--archive --llvm

clean:
	-rm ./jlc
	-rm -r $(SRCDIR)/Javalette/Doc.txt
	-rm -r $(SRCDIR)/Javalette/Print.hs
	-rm -r $(SRCDIR)/Javalette/Skel.hs
	-rm -r $(SRCDIR)/Javalette/Test.hs
	-rm -r .stack-work/
	-rm stack.*
