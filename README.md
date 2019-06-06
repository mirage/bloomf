# Bloomf - Efficient Bloom filters for OCaml [![Build Status](https://travis-ci.org/mirage/bloomf.svg)](https://travis-ci.org/mirage/bloomf)

## Benchmarks

Micro benchmarks are provided for `create`, `add` and `mem` operations.
Expected error rate is 0.01.

|name                             |  major-allocated          |  minor-allocated          |  monotonic-clock          |
|---------------------------------|---------------------------|---------------------------|---------------------------|
|  bloomf/add 10000               |              0.0045 mw/run|              26.0055 w/run|            469.9798 ns/run|
|  bloomf/add 100000              |              0.0044 mw/run|              26.0054 w/run|            468.8711 ns/run|
|  bloomf/add 1000000             |              0.0044 mw/run|              26.0054 w/run|            465.9688 ns/run|
|  bloomf/create 10000            |           1548.2850 mw/run|             225.1115 w/run|          10004.2279 ns/run|
|  bloomf/create 100000           |          15464.1941 mw/run|             261.4265 w/run|          52229.0413 ns/run|
|  bloomf/create 1000000          |         154617.1529 mw/run|             217.6108 w/run|         584479.5283 ns/run|
|  bloomf/find (absent) 10000     |              0.0017 mw/run|               8.0038 w/run|            354.4163 ns/run|
|  bloomf/find (absent) 100000    |              0.0017 mw/run|               8.0038 w/run|            358.0275 ns/run|
|  bloomf/find (absent) 1000000   |              0.0025 mw/run|              11.0044 w/run|            401.7930 ns/run|
|  bloomf/find (present) 10000    |              0.0048 mw/run|              26.0060 w/run|            525.7296 ns/run|
|  bloomf/find (present) 100000   |              0.0046 mw/run|              26.0057 w/run|            480.7180 ns/run|
|  bloomf/find (present) 1000000  |              0.0044 mw/run|              26.0054 w/run|            465.8766 ns/run|

They preform OLS regression analysis using the development version of
[bechamel](https://github.com/dinosaure/bechamel).
To reproduce them, pin `bechamel` then run `make bench`.
