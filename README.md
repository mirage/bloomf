# Bloomf - Efficient Bloom filters for OCaml [![Build Status](https://travis-ci.org/pascutto/bloomf.svg)](https://travis-ci.org/pascutto/bloomf)

## Benckmarks

Micro benckmarks are provided for `create`, `add` and `mem` operations. Expected error rate is 0.1.

|name                             |  major-allocated          |  minor-allocated          |  monotonic-clock   |
|---------------------------------|---------------------------|---------------------------|---------------------------|
|  bloomf/add 10000               |              0.0054 mw/run|              24.0088 w/run|            792.1010 ns/run|
|  bloomf/add 100000              |              0.0054 mw/run|              24.0089 w/run|            802.7286 ns/run|
|  bloomf/add 1000000             |              0.0054 mw/run|              24.0089 w/run|            799.0960 ns/run|
|  bloomf/create 10000            |           1547.1388 mw/run|              26.0983 w/run|           8913.9970 ns/run|
|  bloomf/create 100000           |          15461.8732 mw/run|              26.4489 w/run|          59116.7245 ns/run|
|  bloomf/create 1000000          |         154604.2612 mw/run|              27.6991 w/run|         628970.3156 ns/run|
|  bloomf/find (absent) 10000     |              0.0024 mw/run|              10.0073 w/run|            709.9202 ns/run|
|  bloomf/find (absent) 100000    |              0.0024 mw/run|              10.0073 w/run|            700.4462 ns/run|
|  bloomf/find (absent) 1000000   |              0.0024 mw/run|              10.0076 w/run|            731.5756 ns/run|
|  bloomf/find (present) 10000    |              0.0056 mw/run|              25.0088 w/run|            792.7947 ns/run|
|  bloomf/find (present) 100000   |              0.0056 mw/run|              25.0088 w/run|            786.0082 ns/run|
|  bloomf/find (present) 1000000  |              0.0056 mw/run|              25.0090 w/run|            796.0115 ns/run|

They preform OLS regression analysis using the development version of
[bechamel](https://github.com/dinosaure/bechamel).
To reproduce them, pin `bechamel` then run `make bench`.
