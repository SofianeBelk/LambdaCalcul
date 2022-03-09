CC := ocamlbuild
RM ?= rm -f

EXEC := main
TEST := 

SRCDIR := src
BUILDDIR := _build

SRC := $(wildcard $(SRCDIR)/*.ml)

CFLAGS :=   
LDFLAGS := 

.PHONY: native
native:
	$(CC) $(CFLAGS) $(SRCDIR)/$(EXEC).$@

.PHONY: byte
byte:
	$(CC) $(CFLAGS) $(SRCDIR)/$(EXEC).$@

.PHONY: all
all: native byte test


.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

.PHONY: cleanall
cleanall:
	$(RM) -r $(BUILDDIR) $(EXEC).* 
	