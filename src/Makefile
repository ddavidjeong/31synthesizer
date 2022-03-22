all: build

build:
	@dune build

test:
	@dune exec ./test.exe

gui:
	@dune exec ./gui.exe

synth:
	@dune exec ./main.exe

valgrind: build
	valgrind ../_build/default/examples/test.exe --skip-long