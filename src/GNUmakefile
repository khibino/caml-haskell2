
OCAMLFIND_PP_OPT = -syntax camlp4o
OCAMLFIND_PKG_OPT = -package ulex
OCAMLFIND_VERBOSE = -verbose

extra_interfaces = lablgtk2 ulex
extra_archives = lablgtk ulexing

%Ulexer.cmo: %Ulexer.ml
	ocamlfind ocamlc -c -o $@ $(OCAMLFIND_PKG_OPT) $(OCAMLFIND_PP_OPT) $(OCAMLFIND_VERBOSE) $<

%Ulexer.ml.dep: %Ulexer.ml
	ocamlfind ocamldep $(OCAMLFIND_PKG_OPT) $(OCAMLFIND_PP_OPT)  $< > $@

include ocaml_basic.gmk

all: scratch.cma hpt

rebuild: clean all

sources = \
	data.ml \
	symbol.ml \
	token.ml \
	lazyList.ml \
	hsUlexer.ml \
	hsUlexerTest.ml \
	hsLayout.ml \
	hsLayoutTest.ml \
	hsSyntax.ml \
	parser.ml \
	simple.ml \
	parserSimpleTest.ml \
	hsParserState.ml \
	hsParser.ml \
	hsParserTest.ml \
	t.ml

objs = $(sources:.ml=.cmo)
deps = $(sources:.ml=.ml.dep)

include $(deps)

scratch.cma: $(objs)
	ocamlfind ocamlc -a -o $@ $(OCAMLFIND_PKG_OPT) $(OCAMLFIND_VERBOSE) $^

hsParserTestMain_b: scratch.cma hsParserTestMain.cmo
	$(BYTE_LINK.ml) -o $@ $(byte_archives) $^

hpt: hsParserTestMain_b
	cp $< $@

info:
	$(ocamldep) $(sources)

clean:
	$(RM) *.cmo *.cma
	$(RM) *.cmi
	$(RM) *.annot

realclean: clean
	$(RM) *.ml.dep

