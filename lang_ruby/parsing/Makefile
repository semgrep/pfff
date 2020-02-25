TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_ruby

-include $(TOP)/Makefile.config

SRC= build_vars.ml config.ml log.ml utils.ml \
  visitor.ml \
  annotation.ml \
  ast_ruby.ml abbr_ruby.ml ast_ruby_helpers.ml ast_printer.ml \
  typeAnnotLexer.ml typeAnnotParser.ml \
  newParser.ml \
  rubyLexerState.ml \
  newLexer.ml \
  parse_helper.ml \
  parse_ruby.ml \
  test_parsing_ruby.ml unit_parsing_ruby.ml 


SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/dyp $(TOP)/external/getopt \
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


newLexer.ml: newLexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f newLexer.ml
beforedepend:: newLexer.ml

newParser.ml newParser.mli: typeAnnotParser.cmo typeAnnotLexer.cmo

newParser.ml newParser.mli: newParser.dyp
	dypgen --pv-obj --ocamlc '$(INCLUDES)' $<
clean::
	rm -f newParser.ml newParser.mli 
beforedepend:: newParser.ml newParser.mli


#TODO: remove at some point
typeAnnotParser.ml typeAnnotParser.mli: typeAnnotParser.mly
	$(OCAMLYACC) $<
clean::
	rm -f typeAnnotParser.ml typeAnnotParser.mli 
beforedepend:: typeAnnotParser.ml typeAnnotParser.mli


typeAnnotLexer.ml: typeAnnotLexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f typeAnnotLexer.ml
beforedepend:: typeAnnotLexer.ml

#visitor_python.cmo: visitor_python.ml
#	ocamlc $(INCLUDES) -w y -c $<
