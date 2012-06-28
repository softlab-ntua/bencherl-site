---
title: Home
index: 0
---

# What is BenchErl

BenchErl is a publicly available scalability benchmark suite for applications written in Erlang, using the Erlang/OTP system in particular. In contrast to other benchmark suites, which are usually designed to report a particular performance point, our benchmark suite aims to assess scalability, i.e., a set of performance points that show how an application’s performance changes when additional resources (e.g. CPU cores, schedulers, etc.) are added.

# Motivation

The intuitive parallelization model provided by Erlang is one of its most advertised features. However, understanding the behaviour of a parallel Erlang application and most importantly detect the bottlenecks that hinder the exploitation of a large number of processors has not been an easy task. A tool that would help towards this has been missing for Erlang. The features included in BenchErl allow the execution of applications under different parameters, the visualization of results and the extraction of useful conclusions. Hence, it is a first step to better understand the parameters that affect the parallel execution of Erlang applications.

# Key features

* <u>Unique:</u> To our knowledge, this is the only benchmark suite that targets the scalability of Erlang applications.

* <u>Configurable:</u> A large number of parameters that affect the execution of the benchmarks can be easily configured in a single place. The execution of each benchmark with all possible combinations of the parameters is handled by the BenchErl.

* <u>Automated:</u> The collection and verification of results is performed by the suite. Furthermore, plotting of execution times and speedups in diagrams is also automated.

* <u>Extensible:</u> It is straightforward to add new benchmarks and applications to the suite.

