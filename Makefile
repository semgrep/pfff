#############################################################################
# Configuration section
#############################################################################
-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP:=$(shell pwd)

SRC=find_source.ml

TARGET=pfff

OPAMPKG=pfff

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=pfff \
 sgrep spatch \
 stags \
 scheck \
 codequery \
 codeslicer \
 codegraph_build pfff_db \
 pfff_test

ifeq ($(FEATURE_VISUAL), 1)
PROGS+=codemap codegraph
endif

OPTPROGS= $(PROGS:=.opt)

# not a configuration option anymore; used by too many things
JSONCMA=external/deps-netsys/netsys_oothr.cma external/deps-netsys/netsys.cma\
        external/deps-netstring/netstring.cma\
        external/json-wheel/jsonwheel.cma 

ifeq ($(FEATURE_VISUAL),1)
GTKINCLUDE=external/lablgtk2
CAIROINCLUDE=external/cairo2 external/cairo2-gtk
GUIDIRS=commons_wrappers/gui

VISUALDIRS=code_map code_graph
endif

# could be FEATURE_OCAMLGRAPH, or should give dependencies between features
GRAPHCMA=external/ocamlgraph/graph.cma commons_wrappers/graph/lib.cma
GRAPHDIRS=commons_wrappers/graph 

ifeq ($(FEATURE_BYTECODE), 1)
#still? ZIPCMA=external/ocamlzip/zip.cma
EXTLIBCMA=external/deps-extlib/extLib.cma
PTCMA=external/deps-ptrees/ptrees.cma
JAVALIBCMA=external/javalib/lib.cma

BYTECODEDIRS=lang_bytecode/parsing lang_bytecode/analyze
endif

ifeq ($(FEATURE_CMT), 1)
#bugfix: better to use external/compiler-libs otherwise
# the filter-out in Makefile.common will not skip this dir,
# which may lead to recompilation of .cmi in ocaml stdlib
#old: OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs
OCAMLCOMPILERDIR=external/compiler-libs
OCAMLCOMPILERCMA=ocamlcommon.cma

CMTDIRS=lang_cmt/parsing lang_cmt/analyze
endif

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
BASICSYSLIBS=bigarray.cma str.cma unix.cma

# used for sgrep and other small utilities that I dont want to depend
# on too much things
BASICLIBS=commons/lib.cma \
 commons_core/lib.cma \
 $(JSONCMA) \
 globals/lib.cma \
 h_files-format/lib.cma \
 h_program-lang/lib.cma \
 lang_ml/parsing/lib.cma \
  lang_ml/analyze/lib.cma \
 lang_skip/parsing/lib.cma \
 lang_nw/parsing/lib.cma \
 lang_php/parsing/lib.cma \
  lang_php/matcher/lib.cma \
  lang_php/pretty/lib.cma \
 lang_cpp/parsing/lib.cma \
 lang_c/parsing/lib.cma \
  lang_c/analyze/lib.cma \
 lang_clang/parsing/lib.cma \
 lang_java/parsing/lib.cma \
  lang_java/analyze/lib.cma \
 lang_python/parsing/lib.cma \
  lang_python/analyze/lib.cma \
 lang_csharp/parsing/lib.cma \
 lang_rust/parsing/lib.cma \
 lang_erlang/parsing/lib.cma \
 lang_haskell/parsing/lib.cma \
 lang_lisp/parsing/lib.cma \
 lang_html/parsing/lib.cma \
 lang_js/parsing/lib.cma \
  lang_js/analyze/lib.cma \
 lang_css/parsing/lib.cma \
 lang_web/parsing/lib.cma \
 lang_text/lib.cma \
 lang_sql/parsing/lib.cma \
 lang_GENERIC/parsing/lib.cma \
  lang_GENERIC/analyze/lib.cma \
 lang_FUZZY/parsing/lib.cma \
 matcher/lib.cma \
 linter/lib.cma \
 mini/lib.cma

SYSLIBS=bigarray.cma str.cma unix.cma
SYSLIBS+=$(OCAMLCOMPILERCMA)

# use for the other programs
LIBS= commons/lib.cma \
    commons_core/lib.cma \
    commons_ocollection/lib.cma \
       $(JSONCMA) \
       $(GRAPHCMA) \
       $(EXTLIBCMA) $(PTCMA) $(ZIPCMA) $(JAVALIBCMA) \
    globals/lib.cma \
    h_files-format/lib.cma \
    h_version-control/lib.cma \
    h_program-lang/lib.cma \
    h_visualization/lib.cma \
    h_program-visual/lib.cma \
    graph_code/lib.cma \
    lang_ml/parsing/lib.cma \
     lang_ml/analyze/lib.cma \
    lang_skip/parsing/lib.cma \
     lang_skip/analyze/lib.cma \
    $(CMTDIRS:%=%/lib.cma) \
    lang_nw/parsing/lib.cma \
     lang_nw/analyze/lib.cma \
    lang_lisp/parsing/lib.cma \
     lang_lisp/analyze/lib.cma \
    lang_haskell/parsing/lib.cma \
     lang_haskell/analyze/lib.cma \
    lang_php/parsing/lib.cma \
     lang_php/analyze/foundation/lib.cma \
     lang_php/analyze/tools/lib.cma \
     lang_php/analyze/checker/lib.cma \
     lang_php/matcher/lib.cma \
     lang_php/analyze/qa_test/lib.cma \
     lang_php/analyze/visual/lib.cma \
     lang_php/analyze/lib.cma \
     lang_php/pretty/lib.cma \
    lang_sql/parsing/lib.cma \
    lang_js/parsing/lib.cma \
     lang_js/analyze/lib.cma \
    lang_cpp/parsing/lib.cma \
     lang_cpp/analyze/lib.cma \
    lang_c/parsing/lib.cma \
     lang_c/analyze/lib.cma \
    lang_clang/parsing/lib.cma \
     lang_clang/analyze/lib.cma \
    lang_java/parsing/lib.cma \
     lang_java/analyze/lib.cma \
    $(BYTECODEDIRS:%=%/lib.cma) \
    lang_python/parsing/lib.cma \
     lang_python/analyze/lib.cma \
    lang_csharp/parsing/lib.cma \
     lang_csharp/analyze/lib.cma \
    lang_rust/parsing/lib.cma \
     lang_rust/analyze/lib.cma \
    lang_erlang/parsing/lib.cma \
     lang_erlang/analyze/lib.cma \
    lang_text/lib.cma \
    lang_html/parsing/lib.cma \
     lang_html/analyze/lib.cma \
    lang_css/parsing/lib.cma \
    lang_web/parsing/lib.cma \
    lang_GENERIC/parsing/lib.cma \
     lang_GENERIC/analyze/lib.cma \
    lang_FUZZY/parsing/lib.cma \
    linter/lib.cma \
    matcher/lib.cma \
    mini/lib.cma

MAKESUBDIRS=commons commons_ocollection commons_core \
  $(GRAPHDIRS) \
  $(GUIDIRS) \
  globals \
  h_version-control \
  h_visualization \
  h_files-format \
  h_program-lang \
  graph_code \
  h_program-visual \
  lang_ml/parsing \
   lang_ml/analyze \
  lang_skip/parsing \
   lang_skip/analyze \
  $(CMTDIRS) \
  lang_nw/parsing \
   lang_nw/analyze \
  lang_lisp/parsing \
   lang_lisp/analyze \
  lang_haskell/parsing \
   lang_haskell/analyze \
  lang_php/parsing \
   lang_php/matcher \
   lang_php/pretty \
  lang_sql/parsing \
  lang_js/parsing \
   lang_js/analyze \
  lang_cpp/parsing \
   lang_cpp/analyze \
  lang_c/parsing \
   lang_c/analyze \
  lang_clang/parsing \
   lang_clang/analyze \
  lang_java/parsing \
   lang_java/analyze \
  $(BYTECODEDIRS) \
  lang_python/parsing \
   lang_python/analyze \
  lang_csharp/parsing \
   lang_csharp/analyze \
  lang_rust/parsing \
   lang_rust/analyze \
  lang_erlang/parsing \
   lang_erlang/analyze \
  lang_php/analyze \
   lang_php/analyze/foundation \
   lang_php/analyze/checker \
   lang_php/analyze/tools \
   lang_php/analyze/qa_test \
  lang_html/parsing \
   lang_html/analyze \
  lang_css/parsing \
  lang_web/parsing \
  lang_text \
  lang_GENERIC/parsing \
   lang_GENERIC/analyze \
  lang_FUZZY/parsing \
  matcher \
  linter \
  mini \
  metagen \
  $(VISUALDIRS) \
  demos

INCLUDEDIRS=$(MAKESUBDIRS) \
 external/deps-netsys \
 external/json-wheel \
 $(GTKINCLUDE) $(CAIROINCLUDE) \
 commons_wrappers/graph commons_wrappers/gui \
 $(OCAMLCOMPILERDIR)

# cpp causes some 'warning: missing terminating' errors
CLANG_HACK=-Wno-invalid-pp-token
PP=-pp "cpp $(CLANG_HACK) -DFEATURE_BYTECODE=$(FEATURE_BYTECODE) -DFEATURE_CMT=$(FEATURE_CMT)"

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

#note: old: was before all: rec $(EXEC) ... but can not do that cos make -j20
#could try to compile $(EXEC) before rec. So here we force sequentiality.

all:: Makefile.config
	$(MAKE) rec
	$(MAKE) $(PROGS)
opt:
	$(MAKE) rec.opt
	$(MAKE) $(OPTPROGS)
all.opt: opt
top: $(TARGET).top

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done
rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done


$(TARGET): $(BASICLIBS) $(OBJS) main.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^
$(TARGET).opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^
$(TARGET).top: $(LIBS) $(OBJS)
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) threads.cma $^


clean::
	rm -f $(PROGS) $(TARGET).top *.opt
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done

depend::
	set -e; for i in $(MAKESUBDIRS); do echo $$i; $(MAKE) -C $$i depend; done

Makefile.config:
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f globals/config_pfff.ml
	rm -f TAGS
#	find -name ".#*1.*" | xargs rm -f

# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)

#------------------------------------------------------------------------------
# stags targets (was pfff_tags)
#------------------------------------------------------------------------------

# I've added -linkall just to debug possible issues in wrong 
# order of the SRC files in the library
# (I had a linking error in efuns, which require -linkall)
stags: $(LIBS) $(OBJS) main_stags.cmo
	$(OCAMLC) -linkall $(CUSTOM) -o $@ $(SYSLIBS) $^
stags.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_stags.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

#------------------------------------------------------------------------------
# sgrep/spatch targets
#------------------------------------------------------------------------------

sgrep: $(BASICLIBS) $(OBJS) main_sgrep.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
sgrep.opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main_sgrep.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

spatch: $(BASICLIBS) $(OBJS) main_spatch.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
spatch.opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main_spatch.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

#------------------------------------------------------------------------------
# scheck targets
#------------------------------------------------------------------------------

scheck: $(LIBS) $(OBJS) main_scheck.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
scheck.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_scheck.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

#------------------------------------------------------------------------------
# codequery targets
#------------------------------------------------------------------------------

codequery: $(LIBS) $(OBJS) main_codequery.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codequery.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) $(OPTOBJS) main_codequery.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^

#------------------------------------------------------------------------------
# codeslicer targets
#------------------------------------------------------------------------------

codeslicer: $(LIBS) $(OBJS) main_codeslicer.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codeslicer.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_codeslicer.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

#------------------------------------------------------------------------------
# pfff_db targets
#------------------------------------------------------------------------------

pfff_db: $(LIBS) $(OBJS) main_db.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_db.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) $(OPTOBJS) main_db.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^

#------------------------------------------------------------------------------
# codemap target (was pfff_visual)
#------------------------------------------------------------------------------
SYSLIBS_CM= \
 external/lablgtk2/lablgtk.cma \
 external/cairo2/cairo.cma \
 external/cairo2-gtk/cairo_gtk.cma
OBJS_CM=code_map/lib.cma

GTKLOOP=gtkThread.cmo

codemap: $(LIBS) commons_wrappers/gui/lib.cma $(OBJS_CM) $(OBJS) main_codemap.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma \
            $(SYSLIBS_CM) $(GTKLOOP) $^

codemap.opt: $(LIBS:.cma=.cmxa) commons_wrappers/gui/lib.cmxa $(OBJS_CM:.cma=.cmxa) $(OPTOBJS) main_codemap.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa\
          $(SYSLIBS_CM:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^


#------------------------------------------------------------------------------
# codegraph (was pm_depend)
#------------------------------------------------------------------------------
SYSLIBS_CG=$(SYSLIBS_CM)
OBJS_CG=code_graph/lib.cma

codegraph: $(LIBS) commons_wrappers/gui/lib.cma $(OBJS_CG) $(OBJS) main_codegraph.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma \
           $(SYSLIBS_CG) $(GTKLOOP) $^
codegraph.opt: $(LIBS:.cma=.cmxa) commons_wrappers/gui/lib.cmxa $(OBJS_CG:.cma=.cmxa) $(OPTOBJS) main_codegraph.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa\
          $(SYSLIBS_CG:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

# far simpler dependencies
codegraph_build: $(LIBS) $(OBJS) main_codegraph_build.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codegraph_build.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_codegraph_build.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

#------------------------------------------------------------------------------
# pfff_test targets
#------------------------------------------------------------------------------
pfff_test: $(LIBS) $(OBJS) main_test.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_test.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_test.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^


##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################
VERSION=$(shell cat globals/config_pfff.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

install: all
	ocamlfind install $(OPAMPKG) META
uninstall: all
	ocamlfind remove $(OPAMPKG)

#old:
# note: don't remove DESTDIR, it can be set by package build system like ebuild
#	mkdir -p $(DESTDIR)$(BINDIR)
#	mkdir -p $(DESTDIR)$(SHAREDIR)
#	cp -a $(PROGS) $(DESTDIR)$(BINDIR)
#	cp -a data $(DESTDIR)$(SHAREDIR)
#	@echo ""
#	@echo "You can also install pfff by copying the programs"
#	@echo "available in this directory anywhere you want and"
#	@echo "give it the right options to find its configuration files."
#
#uninstall:
#	rm -rf $(DESTDIR)$(SHAREDIR)/data

# Some of those libraries are needed by efuns, magicator, syncweb
INSTALL_SUBDIRS= \
  commons commons_core commons_ocollection commons_wrappers/graph \
  globals \
  h_program-lang    matcher linter \
  h_version-control h_files-format h_visualization \
  graph_code \
  lang_ml/parsing  lang_ml/analyze \
  lang_cpp/parsing lang_cpp/analyze \
  lang_c/parsing lang_c/analyze \
  lang_nw/parsing  lang_nw/analyze\
  lang_java/parsing  lang_java/analyze\
  lang_js/parsing  lang_js/analyze\
  lang_python/parsing  lang_python/analyze\
  lang_GENERIC/parsing  lang_GENERIC/analyze\
  lang_FUZZY/parsing \


install-libs:: all all.opt
	set -e; for i in $(INSTALL_SUBDIRS); do echo $$i; $(MAKE) -C $$i install-lib; done

uninstall-libs::
	set -e; for i in $(INSTALL_SUBDIRS); do echo $$i; $(MAKE) -C $$i uninstall-lib; done

reinstall-libs:
	make uninstall-libs
	make install-libs

version:
	@echo $(VERSION)

##############################################################################
# Package rules
##############################################################################
PACKAGE=$(TARGET)-$(VERSION)
TMP=/tmp

package:
	make srctar

srctar:
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE).tgz $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)

#todo? automatically build binaries for Linux, Windows, etc?
#http://stackoverflow.com/questions/2689813/cross-compile-windows-64-bit-exe-from-linux

#TODO: making an OPAM package:
# - git push from pfff to github
# - make a new release on github: https://github.com/facebook/pfff/releases
# - get md5sum of new archive
# - update opam file in opam-repository/pfff-xxx/
# - test locally?
# - commit, git push
# - do pull request on github

##############################################################################
# Website rules
##############################################################################
# see also ~/github/pfff-wiki/
WEBSITE=/home/pad/mobile/homepage/software/project-pfff

gen-html:
	emacs -l ~/.emacs --eval "(progn (htmlize-many-files '(\"changes.txt\")) (kill-emacs))"

website:
	cp $(TMP)/$(PACKAGE).tgz                $(WEBSITE)
#	make gen-html
#	cp changes.txt.html $(WEBSITE)/changes-$(VERSION).html

##############################################################################
# Developer rules
##############################################################################
.PHONY:: tags graph prolog  db layers visual   tests test .merlin

.merlin:
	@echo '# -*- sh -*-' > .merlin
	@echo '# Auto-generated by make .merlin' >> .merlin
	@set -e; for i in $(INCLUDEDIRS); do echo "S $$i" >> .merlin || exit 1; done
	@set -e; for i in $(INCLUDEDIRS); do echo "B $$i" >> .merlin || exit 1; done
	@echo 'B external/FOR_MERLIN/**' >> .merlin
	@echo 'S external/FOR_MERLIN/**' >> .merlin


graph:
	./codegraph_build -lang cmt -symlinks -derived_data -verbose .
prolog:
	./codequery.opt -lang cmt -build .
	mv facts.pl facts_pl

#old: superseded by codegraph -derived_data above
tags:
	./stags.opt -lang cmt .
db:
	./pfff_db.opt -db_of_graph_code graph_code.marshall
layers:
	./codegraph.opt -gen_bottomup_layer graph_code.marshall layer_graph_code.json
#./pfff_db_heavy -gen_age_layer /home/pad/local/pfff-for-layers layer_age.marshall
#./pfff_db_heavy -gen_age_layer /home/pad/local/pfff-for-layers layer_age.json

EFUNSCLIENT=/home/pad/github/fork-efuns/efuns_client
visual:
	./codemap -screen_size 3 -filter pfff -efuns_client $(EFUNSCLIENT) -emacs_client /dev/null .
loc:
	./codemap -no_legend -profile -screen_size 3 -filter pfff -test_loc .

tests:
	$(MAKE) rec && $(MAKE) pfff_test
	./pfff_test -verbose all
test:
	make tests

push:
	git push origin master
pull:
	git pull
	cd facebook; git pull

##############################################################################
# Other/Old developer rules
##############################################################################
visual2:
	./codemap -no_legend -profile -ss 2 \
	   -with_info DB_LIGHT.marshall -with_layers . .
visualhead:
	./codemap -ss 1 -ft 0.5 -commitid HEAD

graph2:
	./codegraph.opt -lang ml -build .

#refactoring:
# git grep -l Source_high | xargs perl -p -i -e 's/Source_highlight/Highlight_code/g'

# TODO: replace all of that with a graphviz plugin for codegraph
DSRC=$(SRC)
DIRS= $(filter-out commons external/battery_included/ocamlgtk external/battery_included/ocamlcairo external/ocamlgraph facebook, $(MAKESUBDIRS))
#DIRS=lang_php/parsing
DSRC+=$(DIRS:=/*.ml)
DSRC+=$(wildcard main_*.ml)

#PP1=-pp camlp4o
DOTCOLORS=green,darkgoldenrod2,cyan,red,magenta,yellow,burlywood1,aquamarine,purple,lightpink,salmon,mediumturquoise,black,slategray3

archi:
	ocamldoc $(PP1) -I +threads $(INCLUDES) $(DSRC)  \
	  -dot -dot-reduce -dot-colors $(DOTCOLORS)
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

##############################################################################
# Literate programming
##############################################################################
LPDIRS=\
 h_visualization\
 docs/ocaml \
 lang_php/parsing lang_php/analyze \
 code_map 

lpclean::
	set -e; for i in $(LPDIRS); do echo $$i; $(MAKE) -C $$i $@; done
