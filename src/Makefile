all: build

build:
	@dune build

test:
	@dune exec ./test.exe

synth:
	@dune exec ./synth.exe

valgrind: build
	valgrind ../_build/default/examples/test.exe --skip-long