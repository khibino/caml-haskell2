
OCAMLFIND_ULEX_OPT =
OCAMLFIND_OPT = -package ulex  -package batteries  -package cf -verbose

extra_interfaces = lablgtk2 ulex
extra_archives = lablgtk ulex

%Ulexer.cmo: %Ulexer.ml
	ocamlfind ocamlc -c -o $@ $(OCAMLFIND_OPT) $(OCAMLFIND_ULEX_OPT) -syntax camlp4o $<

include ocaml_basic.gmk

#%.cmo: %.ml
#	ocamlfind ocamlc -c -o $@ $(OCAMLFIND_OPT) $<

all: scratch.cma

rebuild: clean all

sources = \
	symbol.ml \
	token.ml \
	lazyList.ml \
	hsUlexer.ml \
	hsUlexerTest.ml \
	hsLayout.ml \
	hsLayoutTest.ml \
	parser.ml \
	simple.ml \
	hsParser.ml \
	hsParserTest.ml \
	t.ml

objs = $(sources:.ml=.cmo)

scratch.cma: $(objs)
	ocamlfind ocamlc -a -o $@ $(OCAMLFIND_OPT) $(OCAMLFIND_ULEX_OPT) $^

#scratchold.cma: batParserCoExt.cmo token.cmo sSyntax.cmo sUlexer.cmo sCparser.cmo sTestOld.cmo
#	ocamlfind ocamlc -a -o $@ $(OCAMLFIND_OPT) $(OCAMLFIND_ULEX_OPT) $^

clean:
	$(RM) *.cmo
	$(RM) *.cmi
	$(RM) *.annot
