RESULT  = generator
SOURCES = analizator/common.ml parser.ml generator.ml

LIBS = str
OCAMLFLAGS = -unsafe

GEN = generator
LAN = ppjC.lan
OUT = analizator/lang.ml

all: native-code $(OUT)

$(OUT): $(GEN) $(LAN)
	./$(GEN) < $(LAN)

TRASH = $(OUT)

-include OCamlMakefile
