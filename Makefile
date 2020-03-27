SRCDIR = src
BNFC_MAKEFILE_NAME = BNFC_GENERATED.make

all: build

build: grammar
	cabal build

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf \
	--makefile=$(BNFC_MAKEFILE_NAME)
	cd $(SRCDIR) && make --makefile=$(BNFC_MAKEFILE_NAME) all

test:
	@echo "test: Not yet implemented"

clean:
	@echo "clean: Not yet implemented"
