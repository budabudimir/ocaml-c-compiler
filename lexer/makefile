
all: lexer

utils:
	ocamlc -c ../common/utils.ml

tools:
	ocamlc -c tools.ml

lexer: utils lexer.ml tools generator
	bash -c "./generator < simpleC.lan"
	ocamlc -c lang.ml
	ocamlc -I ../common/ utils.cmo str.cma lang.cmo tools.cmo -c lexer.ml -o lexer

generator: generator.ml utils tools
	ocamlc -I ../common/ utils.cmo str.cma tools.cmo generator.ml -o generator

