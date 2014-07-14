
all: generator

utils: utils.ml
	ocamlc -c ../common/utils.ml

common: common.ml
	ocamlc -c common.ml

automaton: automaton.ml
	ocamlc -c automaton.ml

generator: generator.ml utils common automaton
	ocamlc -I ../common/ common.cmo utils.cmo automaton.cmo str.cma generator.ml -o generator 

