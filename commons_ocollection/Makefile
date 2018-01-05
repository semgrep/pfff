##############################################################################
# Variables
##############################################################################

-include ../Makefile.config

INCLUDEDIRS=../commons

LIBNAME=commons_ocollection

SRC=objet.ml \
 ocollection.ml\
 seti.ml setPt.ml \
 oset.ml oassoc.ml osequence.ml oarray.ml  \
 osetb.ml oseth.ml oseti.ml \
 oassocb.ml oassoch.ml \
 ograph.ml ograph_simple.ml ograph_extended.ml ograph2way.ml \
 oassoc_buffer.ml \
 ofullcommon.ml

EXPORTSRC=$(wildcard *.mli)

OCAMLMKLIB=ocamlc -a
OCAMLMKLIBOPT=ocamlopt -a

-include ../commons/Makefile.common
