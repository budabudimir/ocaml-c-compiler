
all: generator

utils:
	ocamlc -c ../common/utils.ml

generator: generator.ml utils
	ocamlc -I ../common utils.cmo generator.ml -o generator

