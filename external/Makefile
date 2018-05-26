TOP=..
# alternatives: battery_included, opam, godi

-include $(TOP)/Makefile.config

MAKESUBDIRS=battery_included/empty battery_included/jsonwheel 

ifeq ($(FEATURE_VISUAL),1)
MAKESUBDIRS+=battery_included/ocamlgtk
MAKESUBDIRS+=battery_included/ocamlcairo
endif

ifeq ($(FEATURE_BYTECODE), 1)
MAKESUBDIRS+=battery_included/ocamlzip
MAKESUBDIRS+=battery_included/extlib
MAKESUBDIRS+=battery_included/ptrees
MAKESUBDIRS+=battery_included/javalib
endif

all:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done
all.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done
clean:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean || exit 1; done
depend:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend || exit 1; done

