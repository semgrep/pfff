all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

#coupling: see .circleci/config.yml
check:
	docker run --rm -v "${PWD}:/src" returntocorp/semgrep:develop --config semgrep.yml --exclude parsing_errors --exclude todo --exclude TODO_more --exclude _build --strict


EFUNSCLIENT=/home/pad/github/fork-efuns/_build/default/efuns_client.exe
visual:
	~/github/codemap/_build/default/bin/main_codemap.exe -screen_size 3 -filter pfff -efuns_client $(EFUNSCLIENT) -emacs_client /dev/null .
loc:
	./codemap -no_legend -profile -screen_size 3 -filter pfff -test_loc .

.PHONY: all clean install test dump
