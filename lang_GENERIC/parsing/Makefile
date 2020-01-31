TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_GENERIC

-include $(TOP)/Makefile.config

SRC= find_source.ml \
     lang.ml \
     ast.ml meta_ast.ml \
     visitor_ast.ml map_ast.ml \
     lib_ast.ml \
     parse_generic.ml \
     test_parsing_generic.ml

SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \
  $(TOP)/lang_python/parsing \
   $(TOP)/lang_python/analyze \
  $(TOP)/lang_js/parsing \
   $(TOP)/lang_js/analyze \
  $(TOP)/lang_c/parsing \
   $(TOP)/lang_c/analyze \
  $(TOP)/lang_java/parsing \
   $(TOP)/lang_java/analyze \
  $(TOP)/lang_ml/parsing \
   $(TOP)/lang_ml/analyze \
  $(TOP)/lang_go/parsing \
   $(TOP)/lang_go/analyze \
  $(TOP)/lang_cpp/parsing \
  $(TOP)/lang_php/parsing \
  $(TOP)/lang_lisp/parsing \

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


visitor_ast.cmo: visitor_ast.ml
	ocamlc $(INCLUDES) -w y -c $<
