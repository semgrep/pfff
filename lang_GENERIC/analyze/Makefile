TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_GENERIC-analyze

SRC= controlflow.ml controlflow_build.ml \
  controlflow_visitor.ml \
  lrvalue.ml \
  dataflow.ml dataflow_reaching.ml dataflow_liveness.ml \
  normalize_ast.ml \
  test_analyze_generic.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     $(TOP)/h_version-control/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/commons_core $(TOP)/commons_ocollection \
   $(TOP)/h_program-lang \
   $(TOP)/globals \
   ../parsing \

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
