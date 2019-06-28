# Bloomf - Efficient Bloom filters for OCaml [![Build Status](https://travis-ci.org/mirage/bloomf.svg)](https://travis-ci.org/mirage/bloomf)

Bloom filters are memory and time efficient data structures allowing
probabilistic membership queries in a set.

A query negative result ensures that the element is not present in the set,
while a positive result might be a false positive, i.e. the element might
not be present and the BF membership query can return true anyway.

Internal parameters of the BF allow to control its false positive rate
depending on the expected number of elements in it.

Online documentation is available [here](https://mirage.github.io/bloomf/).

## Install

`bloomf` is available on opam with `opam install bloomf`.

Alternatively, you can build from sources with `make` or `dune build`.

## Tests

Some of the tests, measuring false positive rate or size estimation, might fail
once in a while since they are randomized. They are thus removed from
`dune runtest` alias.

To run the whole test suite, run `dune build @runtest-rand` instead.

## Benchmarks

Micro benchmarks are provided for `create`, `add`, `mem` and `size_estimate` operations.
Expected error rate is 0.01.

```
╭─────────────────────────────────┬───────────────────────────┬───────────────────────────┬───────────────────────────╮
│name                             │  major-allocated          │  minor-allocated          │  monotonic-clock          │
├─────────────────────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┤
│  bloomf/add 10000               │              0.0058 mw/run│              26.0521 w/run│            469.4458 ns/run│
│  bloomf/add 100000              │              0.0058 mw/run│              26.0521 w/run│            471.2910 ns/run│
│  bloomf/add 1000000             │              0.0060 mw/run│              26.0526 w/run│            470.9914 ns/run│
│  bloomf/create 10000            │           1548.4175 mw/run│             225.5322 w/run│          10149.9990 ns/run│
│  bloomf/create 100000           │          15465.1055 mw/run│             262.3062 w/run│          57264.0710 ns/run│
│  bloomf/create 1000000          │         154620.0872 mw/run│             220.3485 w/run│         596461.1636 ns/run│
│  bloomf/mem (absent) 10000      │              0.0034 mw/run│              14.0450 w/run│            414.6522 ns/run│
│  bloomf/mem (absent) 100000     │              0.0028 mw/run│              11.0523 w/run│            424.9552 ns/run│
│  bloomf/mem (absent) 1000000    │              0.0031 mw/run│              14.0463 w/run│            414.0829 ns/run│
│  bloomf/mem (present) 10000     │              0.0059 mw/run│              26.0551 w/run│            494.2303 ns/run│
│  bloomf/mem (present) 100000    │              0.0059 mw/run│              26.0551 w/run│            489.7722 ns/run│
│  bloomf/mem (present) 1000000   │              0.0060 mw/run│              26.0557 w/run│            491.3580 ns/run│
│  bloomf/size_estimate 10000     │              0.0000 mw/run│               0.6652 w/run│          15804.1932 ns/run│
│  bloomf/size_estimate 100000    │              0.0000 mw/run│               2.0704 w/run│         157101.0313 ns/run│
│  bloomf/size_estimate 1000000   │              0.0000 mw/run│               6.3913 w/run│        1655734.1719 ns/run│
╰─────────────────────────────────┴───────────────────────────┴───────────────────────────┴───────────────────────────╯
```

They preform OLS regression analysis using the development version of
[bechamel](https://github.com/dinosaure/bechamel).
To reproduce them, pin `bechamel` then run `make bench`.
