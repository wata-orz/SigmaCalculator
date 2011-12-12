# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = util.ml syntax.ml parser.mly lexer.mll sigma.ml emit.ml emitL.ml emitM.ml emitB.ml main.ml

# the name of the resulting executable
RESULT  = sigma

# make target (see manual) : byte-code, debug-code, native-code, ...
all: native-code

include OCamlMakefile
