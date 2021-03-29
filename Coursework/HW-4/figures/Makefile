all: figlexer.mll figparser.mly figures.ml figgen.ml
	ocamllex figlexer.mll
	ocamlyacc figparser.mly
	ocamlc -o figgen figures.ml figparser.mli figlexer.ml figparser.ml figgen.ml

clean:
	rm *.cmo
	rm *.cmi
	rm figgen

full-clean: clean
	rm figlexer.ml
	rm figparser.ml
	rm figparser.mli
