all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

.PHONY: all clean install test dump
