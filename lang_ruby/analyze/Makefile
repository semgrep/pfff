TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_ruby-analyze

SRC= dQueue.ml log.ml \
  il_ruby.ml meta_il_ruby.ml \
  visitor.ml il_ruby_helpers.ml \
  il_ruby_build.ml \
  highlight_ruby.ml \
  test_analyze_ruby.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons_core \
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
