all: parser.ml parser.mli lexer.ml types.ml print.ml interp.ml main.ml
	ocamlc -o microml types.ml parser.mli lexer.ml parser.ml print.ml interp.ml main.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.ml: parser.mly
	ocamlyacc parser.mly

clean:
	rm -f *.cmo
	rm -f *.cmi
	-- rm microml
	-- rm lexer.ml
	-- rm parser.mli
	-- rm parser.ml
