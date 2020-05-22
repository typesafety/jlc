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

submissionC:
	tar -czf partC-REPLACETHIS.tar.gz doc lib src jlc.cabal Makefile

testC:
	tar -czf tester/partC-999.tar.gz doc lib src jlc.cabal Makefile
	cd tester && python3 testing.py partC-999.tar.gz \
		--archive --llvm -x arrays1

clean:
	-rm ./jlc
	-rm -r $(SRCDIR)/Javalette/Doc.txt
	-rm -r $(SRCDIR)/Javalette/Print.hs
	-rm -r $(SRCDIR)/Javalette/Skel.hs
	-rm -r $(SRCDIR)/Javalette/Test.hs
	-rm -r .stack-work/
	-rm stack.*
