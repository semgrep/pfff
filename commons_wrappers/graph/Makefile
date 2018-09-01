TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
# for OPAM
LIBNAME=pfff-commons-graph

SRC= graphe.ml

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/ocamlgraph/ \

#needed for make top
LIBS=../../external/ocamlgraph/graph.cma
SYSLIBS= str.cma unix.cma

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
