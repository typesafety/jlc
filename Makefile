SRCDIR = src

all:
	@echo "all: Not yet implemented"

build:
	@echo "build: Not yet implemented"

grammar:
	bnfc -o $(SRCDIR) -d $(SRCDIR)/Javalette.cf

test:
	@echo "test: Not yet implemented"

clean:
	@echo "clean: Not yet implemented"
