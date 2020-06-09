TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_ruby

-include $(TOP)/Makefile.config

SRC= utils_ruby.ml \
  ast_ruby.ml \
  ast_ruby_helpers.ml \
  meta_ast_ruby.ml \
  parser_ruby_helpers.ml \
  parser_ruby.ml \
  lexer_parser_ruby.ml \
  lexer_ruby.ml \
  token_helpers_ruby.ml \
  parse_ruby.ml \
  lib_parsing_ruby.ml \
  test_parsing_ruby.ml unit_parsing_ruby.ml 


SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/dyp \
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


lexer_ruby.ml: lexer_ruby.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_ruby.ml
beforedepend:: lexer_ruby.ml

parser_ruby.ml parser_ruby.mli: parser_ruby.dyp
	dypgen --pv-obj --ocamlc '$(INCLUDES) -w y' $<
clean::
	rm -f parser_ruby.ml parser_ruby.mli 
#beforedepend:: parser_ruby.ml parser_ruby.mli

parser_ruby.cmo: parser_ruby.ml
	ocamlc $(INCLUDES) -w y -c $<
