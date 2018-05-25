
OCAMLC=ocamlc
OPT=-g -dtypes
INCLUDES= -I xml-light xml-light.cma 
extract_stubs: extract_prototype.ml xml-light/xml-light.cma
	$(OCAMLC) $(OPT) $(INCLUDES) -o $@ $^

test: extract_stubs
	./extract_stubs /usr/share/gir-1.0/Gtk-2.0.gir 

clean:
	rm -f extract_stubs *.annot extract_prototype.cm* ml_stubs.c stubs.ml
	$(MAKE) -C xml-light clean

xml-light/xml-light.cma:
	$(MAKE) -C xml-light

