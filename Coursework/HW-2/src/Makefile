all: dfa nfa regex

dfa: dfa.ml dfaMain.ml parseDFA.ml
	ocamlc -o dfa parseDFA.ml dfa.ml dfaMain.ml

nfa: nfa.ml nfaMain.ml parseNFA.ml
	ocamlc -o nfa parseNFA.ml nfa.ml nfaMain.ml

regex: nfa.ml regex.ml parseNFA.ml regexMain.ml
	ocamlc -o regex parseNFA.ml nfa.ml regex.ml regexMain.ml

clean:
	rm *.cmo *.cmi dfa nfa regex
