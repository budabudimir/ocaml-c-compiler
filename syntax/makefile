
all: generator

utils:
	ocamlc -c ../common/utils.ml

common:
	ocamlc -c common.ml

generator: generator.ml utils common
	ocamlc -I ../common/ common.cmo utils.cmo str.cma generator.ml -o generator 

