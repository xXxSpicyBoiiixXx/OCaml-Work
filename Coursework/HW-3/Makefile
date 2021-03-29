all: types.ml print.ml parse.ml topy.ml main.ml
	ocamlc -o microml types.ml print.ml parse.ml topy.ml main.ml

assert: types.ml print.ml parse.ml
	ocamlc -o microml-test types.ml print.ml parse.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm microml
