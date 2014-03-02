
all: generator

utils:
	ocamlc -c ../common/utils.ml

lexer: utils lexer.ml
	ocamlc -I ../common utils.cmo str.cma -c lexer.ml -o lexer

generator: generator.ml utils lexer
	ocamlc -I ../common utils.cmo str.cma lexer.cmo generator.ml -o generator

