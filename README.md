# Bloomf - Efficient Bloom filters for OCaml [![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fmirage%2Fbloomf%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/mirage/bloomf)
Bloom filters are memory and time efficient data structures allowing
probabilistic membership queries in a set.

A query negative result ensures that the element is not present in the set,
while a positive result might be a false positive, i.e. the element might not be
present and the BF membership query can return true anyway.

Internal parameters of the BF allow to control its false positive rate depending
on the expected number of elements in it.

Online documentation is available [here](https://mirage.github.io/bloomf/).

## Install

The latest version of `bloomf` is available on opam with `opam install bloomf`.

Alternatively, you can build from sources with `make` or `dune build`.

## Tests

Some of the tests, measuring false positive rate or size estimation, might fail
once in a while since they are randomized. They are thus removed from `dune
runtest` alias.

To run the whole test suite, run `dune build @runtest-rand` instead.

## Benchmarks

Micro benchmarks are provided for `create`, `add`, `mem` and `size_estimate`
operations. Expected error rate is 0.01.

They preform OLS regression analysis using the development version of
[bechamel](https://github.com/dinosaure/bechamel). To reproduce them, pin
`bechamel` then run `dune build @bench`.
