TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_js-matcher

SRC= metavars_js.ml \
     ast_js_vs_js.ml \
     matching_js.ml \
     sgrep_js.ml \

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/commons_ocollection \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/external/ \
   $(TOP)/matcher \
   ../parsing \
   ../analyze

##############################################################################
# Generic variables
##############################################################################
#WARNING_FLAGS=-w +A-4-29-6-45-32
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

#BUG in ocamlc, false positive on unused value
#php_vs_php.cmo: php_vs_php.ml
#	$(OCAMLC) -w -A -c $<
