# $Id: Makefile,v 1.4 2008-04-01 22:18:24 rjones Exp $

PACKAGE	= ocaml-bitmatch
VERSION	= 0.1

OCAMLFIND = ocamlfind
OCAMLMKLIB = ocamlmklib

OCAMLCFLAGS = -g
OCAMLCPACKAGES =
OCAMLOPTFLAGS =
OCAMLOPTPACKAGES =

EXAMPLES := $(wildcard examples/*.ml)

TESTS	:= $(patsubst %.ml,%,$(wildcard tests/*.ml))

all:	pa_bitmatch.cmo bitmatch.cma bitmatch.cmxa

pa_bitmatch.cmo: pa_bitmatch.ml
	ocamlfind ocamlc -I +camlp4 camlp4lib.cma -pp camlp4of.opt -c $< -o $@

bitmatch.cma: bitmatch.cmo
	$(OCAMLFIND) ocamlc -a -o $@ $^

bitmatch.cmxa: bitmatch.cmx
	$(OCAMLFIND) ocamlopt -a -o $@ $^

test: pa_bitmatch.cmo bitmatch.cma
	@for f in $(TESTS); do \
	  echo Test: $$f; \
	  $(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -pp "camlp4o pa_bitmatch.cmo" \
	    -I . bitmatch.cma $$f.ml -o $$f; \
	  if [ $$? -ne 0 ]; then exit 1; fi; \
	  $$f; \
	  if [ $$? -ne 0 ]; then exit 1; fi; \
	done

print-tests: pa_bitmatch.cmo
	@for f in $(TESTS); do \
	  echo Test: $$f.ml; \
	  cmd="camlp4o pa_bitmatch.cmo -printer pr_o.cmo $$f.ml"; \
	  echo $$cmd; \
	  $$cmd; \
	done

print-examples: pa_bitmatch.cmo
	@for f in $(EXAMPLES); do \
	  echo Example: $$f; \
	  camlp4o pa_bitmatch.cmo -printer pr_o.cmo $$f; \
	done

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLCPACKAGES) -c $<
.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLCPACKAGES) -c $<
.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTPACKAGES) -c $<

depend: .depend

.depend: bitmatch.ml bitmatch.mli
	rm -f .depend
	$(OCAMLFIND) ocamldep $(OCAMLCPACKAGES) $^ > $@

ifeq ($(wildcard .depend),.depend)
include .depend
endif

# Distribution.

dist:
	$(MAKE) check-manifest
	rm -rf $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)
	tar -cf - -T MANIFEST | tar -C $(PACKAGE)-$(VERSION) -xf -
	$(INSTALL) -m 0755 configure $(PACKAGE)-$(VERSION)/
	tar zcf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	rm -rf $(PACKAGE)-$(VERSION)
	ls -l $(PACKAGE)-$(VERSION).tar.gz

check-manifest:
	@for d in `find -type d -name CVS`; do \
	b=`dirname $$d`/; \
	awk -F/ '$$1 != "D" {print $$2}' $$d/Entries | \
	sed -e "s|^|$$b|" -e "s|^\./||"; \
	done | sort > .check-manifest; \
	sort MANIFEST > .orig-manifest; \
	diff -u .orig-manifest .check-manifest; rv=$$?; \
	rm -f .orig-manifest .check-manifest; \
	exit $$rv

.PHONY: depend dist check-manifest dpkg doc print-examples print-tests test

.SUFFIXES:      .cmo .cmi .cmx .ml .mli .mll
