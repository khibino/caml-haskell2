
ocamlfind_pp_opt = -syntax camlp4o
ocamlfind_pkg_opt = -package ulex -package extlib
ocamlfind_verbose = -verbose

extra_interfaces = lablgtk2 ulex extlib
extra_archives = lablgtk ulexing extLib

%Ulexer.cmo: %Ulexer.ml
	ocamlfind ocamlc -c -o $@ $(ocamlfind_pkg_opt) $(ocamlfind_pp_opt) $(ocamlfind_verbose) $<

%Ulexer.ml.dep: %Ulexer.ml
	ocamlfind ocamldep $(ocamlfind_pkg_opt) $(ocamlfind_pp_opt)  $< > $@

include ocaml_basic.gmk

all: scratch.cma hpt

rebuild: clean all

olds = \
	data.ml \
	symbol.ml \
	token.ml \
	lazyList.ml \
	hsUlexer.ml \
	hsUlexerTest.ml \
	hsLayout.ml \
	hsLayoutTest.ml \
	hsSyntaxOld.ml \
	parserOld.ml \
	simple.ml \
	parserSimpleTestOld.ml \
	hsParserStateOld.ml \
	hsParserOld.ml \


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
	parserDriver.ml \
	parserSimpleTest.ml \
	hsParserState.ml \
	hsParser.ml \
	hsParserTest.ml \
	t.ml

objs = $(sources:.ml=.cmo)
deps = $(sources:.ml=.ml.dep)

include $(deps)

scratch.cma: $(objs)
	ocamlfind ocamlc -a -o $@ $(ocamlfind_pkg_opt) $(ocamlfind_verbose) extLib.cma $^

hsParserTestMain_b: scratch.cma hsParserTestMain.cmo
	$(BYTE_LINK.ml) -o $@ $(byte_archives) $^

hpt: hsParserTestMain_b
	cp $< $@

commit: rebuild
	hg commit

info:
	$(ocamldep) $(sources)

clean:
	$(RM) *.cmo *.cma
	$(RM) *.cmi
	$(RM) *.annot

realclean: clean
	$(RM) *.ml.dep

