
OCAMLFIND_ULEX_OPT =
OCAMLFIND_OPT = -package ulex  -package batteries  -package cf -verbose

extra_interfaces = ulex str
extra_archives = ulex str

%Ulexer.cmo: %Ulexer.ml
	ocamlfind ocamlc -c -o $@ $(OCAMLFIND_OPT) $(OCAMLFIND_ULEX_OPT) -syntax camlp4o $<

include ocaml_basic.gmk

#%.cmo: %.ml
#	ocamlfind ocamlc -c -o $@ $(OCAMLFIND_OPT) $<

all: scratch.cma

scratch.cma: symbol.cmo token.cmo hsUlexer.cmo hsUlexerTest.cmo lazyList.cmo parser.cmo simple.cmo
	ocamlfind ocamlc -a -o $@ $(OCAMLFIND_OPT) $(OCAMLFIND_ULEX_OPT) $^

#scratchold.cma: batParserCoExt.cmo token.cmo sSyntax.cmo sUlexer.cmo sCparser.cmo sTestOld.cmo
#	ocamlfind ocamlc -a -o $@ $(OCAMLFIND_OPT) $(OCAMLFIND_ULEX_OPT) $^

clean:
	$(RM) *.cmo
	$(RM) *.cmi
	$(RM) *.annot
