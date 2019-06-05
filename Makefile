.PHONY: all test bench doc clean

all:
	dune build

test:
	dune runtest

bench:
	dune build @bench

clean:
	dune clean

doc:
	dune build @doc
