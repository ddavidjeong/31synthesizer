.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f synth.zip
	zip -r synth.zip . -x@exclude.lst

clean:
	dune clean
	rm -f synth.zip

main:
	OCAMLRUNPARAM=b dune exec bin/main.exe

terminal:
	OCAMLRUNPARAM=b dune exec bin/terminal.exe

docs:
	dune build @doc
