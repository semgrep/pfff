graph: 
	~/pfff/codegraph  -symlinks -lang php -build .
	cat pfff.log

check:
	~/pfff/scheck -emacs -with_graph_code graph_code.marshall .

clean:
	rm -f graph_code.marshall pfff.log graph_code.marshall.opti
