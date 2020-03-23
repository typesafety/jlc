SRCDIR = src
BNFC_MAKEFILE_NAME = BNFC_GENERATED.make

all:
	@echo "all: Not yet implemented"

build:
	@echo "build: Not yet implemented"

grammar:
	bnfc -o $(SRCDIR) --makefile=$(BNFC_MAKEFILE_NAME) -d $(SRCDIR)/Javalette.cf
	cd $(SRCDIR) && make --makefile=$(BNFC_MAKEFILE_NAME) all

test:
	@echo "test: Not yet implemented"

clean:
	@echo "clean: Not yet implemented"
