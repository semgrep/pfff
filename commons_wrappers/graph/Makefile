TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= graphe.ml
SYSLIBS= str.cma unix.cma
LIBS=../../external/ocamlgraph/graph.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/ocamlgraph/ \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

##############################################################################
# install
##############################################################################
LIBNAME=pfff-commons-graph
EXPORTSRC=graphe.mli

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a *.cmi $(EXPORTSRC) 
