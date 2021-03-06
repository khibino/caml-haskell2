
ocamlfind_pp_opt = -syntax camlp4o
ocamlfind_pkg_opt = -package ulex -package extlib
ocamlfind_verbose = -verbose

extra_interfaces = lablgtk2 ulex extlib
extra_archives = lablgtk ulexing extLib

programs = hsParserTestMain_b hpt

%Ulexer.cmo: %Ulexer.ml
	ocamlfind ocamlc -c -o $@ $(ocamlfind_pkg_opt) $(ocamlfind_pp_opt) $(ocamlfind_verbose) $<

%Ulexer.ml.dep: %Ulexer.ml
	ocamlfind ocamldep $(ocamlfind_pkg_opt) $(ocamlfind_pp_opt)  $< > $@

include ocaml_basic.gmk

all: scratch.cma $(programs)

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
	parserDriver.ml \
	parserDerive.ml \
	parserSimpleTest.ml \
	hsParserState.ml \
	hsParser.ml \
	hsParserTest.ml \
	t.ml

objs = $(sources:.ml=.cmo)
deps = $(sources:.ml=.ml.dep)

test_sources = \
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
	parserDerive.ml \
	parserSimpleTest.ml \

test_objs = $(test_sources:.ml=.cmo)

include $(deps)

scratch.cma: $(objs)
	ocamlfind ocamlc -a -o $@ $(ocamlfind_pkg_opt) $(ocamlfind_verbose) extLib.cma $^

test.cma: $(test_objs)
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
	$(RM) $(programs)
	$(RM) *.cmo *.cma
	$(RM) *.cmi
	$(RM) *.annot

realclean: clean
	$(RM) *.ml.dep

