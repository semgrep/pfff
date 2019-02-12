TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=cairo_helpers3.ml \
    model3.ml controller3.ml \
    view_matrix.ml view_overlays3.ml \
    view3.ml

SYSLIBS=str.cma unix.cma dbm.cma threads.cma  lablgtk.cma

LIBS=$(TOP)/commons/lib.cma $(TOP)/commons_wrappers/gui/lib.cma \

INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
    $(TOP)/commons_wrappers/gui \
	$(TOP)/h_program-lang \
	$(TOP)/graph_code \
	$(TOP)/h_visualization \
	$(TOP)/h_program-visual \
    $(TOP)/external/cairo2 \
    $(TOP)/external/lablgtk2 \
    $(TOP)/external/cairo2-gtk \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma 
all.opt:: $(TARGET).cmxa
opt: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

clean::
	rm -f $(TARGET).top

##############################################################################
# Generic rules
##############################################################################
