LIBS=netstring netsys netsys_oothr

all: $(LIBS:%=%.cma)
all.opt: $(LIBS:%=%.cmxa)

# ocamlmklib should be used only when you have some C files involved
# otherwise it does weird things

%.cma: empty.cmo
	ocamlc -a -o $@ $^
#	ocamlmklib -o $* $^

%.cmxa: empty.cmx
	ocamlopt -a -o $@ $^
#	ocamlmklib -o $* $^

%.cmo: %.ml
	ocamlc -c $^

%.cmx: %.ml
	ocamlopt -c $^

clean:
	rm -f *.cm* *.a *.o

depend:
