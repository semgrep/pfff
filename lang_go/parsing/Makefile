TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_go

-include $(TOP)/Makefile.config

SRC= ast_go.ml meta_ast_go.ml \
     visitor_go.ml \
     lib_parsing_go.ml \
     parser_go.ml lexer_go.ml \
     token_helpers_go.ml \
     parsing_hacks_go.ml \
     parse_go.ml \
     test_parsing_go.ml unit_parsing_go.ml

SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

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


lexer_go.ml: lexer_go.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_go.ml
beforedepend:: lexer_go.ml


parser_go.ml parser_go.mli: parser_go.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_go.ml parser_go.mli parser_go.output
beforedepend:: parser_go.ml parser_go.mli

visitor_go.cmo: visitor_go.ml
	$(OCAMLC) -w y -c $<
