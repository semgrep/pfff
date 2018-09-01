TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
LIBNAME=pfff-lang_ml-visual

SRC= highlight_ml.ml

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/h_program-lang  \
   $(TOP)/lang_ml/parsing \

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \

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
$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top
