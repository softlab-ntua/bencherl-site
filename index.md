---
title: Home
index: 0
---

BenchErl is a publicly available scalability benchmark suite for applications 
written in Erlang. In contrast to other benchmark suites, which are usually 
designed to report a particular performance point, our benchmark suite aims to 
assess scalability, i.e., a set of performance points that show how an 
application’s performance changes when additional resources (e.g. CPU cores, 
schedulers, etc.) are added.

#### MOTIVATION

The concurrency model of Erlang is one of its most advertised features. However,
understanding the behaviour of a highly concurrent Erlang application and most 
importantly detecting the bottlenecks that hinder the exploitation of a large 
number of CPU cores has not been an easy task. A tool that would help towards 
this direction has been missing for Erlang.

The features included in BenchErl allow the execution of applications in various
execution environments, the visualization of the results, and the extraction of
useful conclusions. Hence, it is a tool that might help the Erlang community 
make a first step to better understand the parameters that affect the parallel 
execution of Erlang applications.

#### KEY FEATURES

* **Unique**: BenchErl is the only benchmark suite that targets the scalability of Erlang applications.

* **Configurable**: BenchErl allows the configuration of a large number of parameters that might affect the execution of a benchmark. The execution of the benchmark with all possible combinations of these parameters is handled by BenchErl.

* **Automated**: BenchErl handles the collection, the execution and the visual presentation of the benchmark execution results.

* **Extendable**: It is straightforward to add new benchmarks and applications to BenchErl.

