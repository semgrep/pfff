TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_GENERIC-analyze

SRC= \
  Naming_AST.ml Normalize_AST.ml \
  controlflow.ml controlflow_build.ml \
  controlflow_visitor.ml \
  lrvalue.ml \
  Dataflow.ml dataflow_reaching.ml dataflow_liveness.ml \
  IL.ml CFG.ml Meta_IL.ml AST_to_IL.ml CFG_build.ml \
  Dataflow_tainting.ml \
  Test_analyze_generic.ml \
  Unit_typing_generic.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
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
