TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= graph_code_cmt_helpers.ml graph_code_cmt.ml \
 unit_analyze_cmt.ml

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons_core \
 $(TOP)/external/compiler-libs \
 $(TOP)/globals \
 $(TOP)/h_program-lang $(TOP)/graph_code \
 $(TOP)/lang_cmt/parsing \
 $(TOP)/lang_ml/parsing \
 $(TOP)/lang_ml/analyze \

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
