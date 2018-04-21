##############################################################################
# Variables
##############################################################################

# if part of pfff/ or other programs with a Makefile.config
-include ../Makefile.config

LIBNAME=commons

# note: if you add a file (a .mli or .ml), dont forget to redo a 'make depend'
SRC=common.ml common2.ml \
 ocaml.ml\
 file_type.ml\
 set_.ml map_.ml \
 dumper.ml oUnit.ml

EXPORTSRC=$(SRC:%.ml=%.mli)

OCAMLMKLIB=ocamlc -a
OCAMLMKLIBOPT=ocamlopt -a
#ocamlmklib, does some weird things when you actually dont have C code

SYSLIBS=unix.cma str.cma

-include Makefile.common

# too many code in pfff assume commons/lib.cma
all:: lib.cma
all.opt: lib.cmxa lib.a

lib.cma: $(LIBNAME).cma
	cp $^ $@
lib.cmxa: $(LIBNAME).cmxa
	cp $^ $@
lib.a: $(LIBNAME).a
	cp $^ $@

##############################################################################
# Developer rules
##############################################################################

clean::
	rm -f gmon.out

forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt

# obsolete, use codegraph instead!
dependencygraph:
	ocamldep *.mli *.ml > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps

dependencygraph2:
	find  -name "*.ml" |grep -v "scripts" | xargs ocamldep -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl -I extra > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps
