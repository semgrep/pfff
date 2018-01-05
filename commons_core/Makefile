##############################################################################
# Variables
##############################################################################

-include ../Makefile.config

INCLUDEDIRS=../commons

LIBNAME=commons_core

SRC=ANSITerminal.ml console.ml \
 int.ml \
 math.ml \
 logger.ml \
 parallel.ml \


EXPORTSRC=$(SRC:%.ml=%.mli)

OCAMLMKLIB=ocamlc -a
OCAMLMKLIBOPT=ocamlopt -a

-include ../commons/Makefile.common
