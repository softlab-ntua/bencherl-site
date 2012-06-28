---
title: Download
index: 3
---

* [Download the source code](#Download)
* [Installation Instructions](#Installation)
* [Supported Platforms](#Platforms)
* [User Manual](#UserManual)
* [Extending the Benchmark Suite](#Extending)

### <a name="Download"></a> DOWNLOAD THE SOURCE CODE

The latest source code for `BenchErl` is freely available on [github](https://github.com/k4t3r1n4/Bencherl).

### <a name="Installation"></a> INSTALLATION INSTRUCTIONS

The installation of BenchErl is straight-forward, as it follows the typical Unix/Linux approach. After downloading the source code, simply enter the directory `BenchErl`
and type `make` at the command prompt:

	~ $ cd BenchErl  
	~ $ make

This will compile both, the synthetic benchmarks and the applications included in the suite. If you would like to build only the benchmarks then you should run:

	~ $ make bench

Finally, if you would like to only build the applications then you have to run:

	~ $ make app

#### <a name="Platforms"></a> SUPPORTED PLATFORMS

The benchmark suite has been tested on the following systems. It should however build and run on other Unix/Linux systems.

* Debian Linux 6.0 x86-64
	* Erlang/OTP version R14A, R14B, R14B01, R14B02, R14B03, R14B04, R15B, R15B01

* Fedora 16 x86-64
	* Erlang/OTP version R14A, R14B, R14B01, R14B02, R14B03, R14B04, R15B, R15B01

### <a name="UserManual"></a> USER MANUAL

The purpose of the benchmark suite is to provide an easy way to execute a number of benchmarks with a combination of parameters. For example, we could be interested in running only the benchmarks
`big` and `bang` on a number of different Erlang/OTP versions and for a number of schedulers, e.g., 1, 2, 4 and 8. The tool we have to use to achieve this is the script `bencherl`
contained in the suite.

This script takes a number of options and (most importantly) reads a configuration file that the user has to alter, in order to specify the benchmarks and the combination of parameters he/she is interested in.
A description of the options accepted by the script and the entries of the configuration file follow.

#### `bencherl` SCRIPT OPTIONS

Only three options are accepted by `bencherl`:

<table border="0" cellpadding="5">
	<tr>
		<td class="option-name">-h:</td>
		<td class="option-description">Displays a short help message and exits.</td>
	</tr>
	<tr>
		<td class="option-name">-l:</td>
		<td class="option-description">Prints a list of all available benchmarks included in the suite.</td>
	</tr>
	<tr>
		<td class="option-name">-m &lt;MNEMONIC&gt;:</td>
		<td class="option-description">The output of a run is saved under a directory that is by default named by the date and time when the run was initiated. This option is used when the user wants to name the output directory differently.</td>
	</tr>
</table>

#### CONFIGURATION

The important part in configuring an execution of a set of benchmarks is performed in the file `conf/run.conf`. This file is a simple BASH script in which the user sets a number of variables that define the behavior of the execution to follow. The following paragraphs explain each of the available options.

<table border="0" cellpadding="5">
	<tr>
		<td class="option-name">USE_LONG_NAMES=[0|1]</td>
		<td class="option-description">If the value of this option is set to 1, Erlang will use [long names](http://www.erlang.org/doc/reference_manual/distributed.html#id82803) to identify the nodes on which each benchmark will execute. Otherwise it will use short names.</td>
	</tr>
	<tr>
		<td class="option-name">MASTER_NODE=&lt;NodeName&gt;</td>
		<td class="option-description">The long or the short name of the node that will act as a master node.</td>
	</tr>
	<tr>
		<td class="option-name">SLAVE_NODES=&lt;NodeName1,NodeName2,...&gt;</td>
		<td class="option-description">A comma separated list of node names that will be assigned in order to slave nodes.</td>
	</tr>
	<tr>
		<td class="option-name">OTPS=&lt;"OTPName1=Path1,OTPName2=Path2,..."&gt;</td>
		<td class="option-description">A comma separated list of Erlang/OTP versions to use when running the benchmarks. A name is assigned to each version, followed by the path leading to it.</td>
	</tr>
	<tr>
		<td class="option-name">ERL_ARGS=&lt;"ARG1=val1,ARG2=val2,..."&gt;</td>
		<td class="option-description">A comma separated list of arguments that have to be passed to Erlang. ARG1, ARG2, ... are arguments accepted by Erlang and val1, val2, ... are the values that will be assigned to them.</td>
	</tr>
	<tr>
		<td class="option-name">INCLUDE_BENCH=&lt;Bench1,Bench2,...&gt;</td>
		<td class="option-description">A comma separated list of the benchmarks that the user wishes to run. If this option is not used then all benchmarks are executed.</td>
	</tr>
	<tr>
		<td class="option-name">EXCLUDE_BENCH=&lt;Bench1,Bench2,...&gt;</td>
		<td class="option-description">A comma separated list of the benchmarks that the user does not wish to run. This option has higher precedence than INCLUDE_BENCH, i.e., if a benchmark is listed in both options, then it will not be run.</td>
	</tr>
	<tr>
		<td class="option-name">COOKIE=&lt;"Value"&gt;</td>
		<td class="option-description">A cookie that will be set on all Erlang nodes started when running a benchmark.</td>
	</tr>
	<tr>
		<td class="option-name">PLOT=[0|1]</td>
		<td class="option-description">If this option is set the collected results will be used to create diagrams of execution time and speedup. The default value is 1.</td>
	</tr>
	<tr>
		<td class="option-name">CHECK_SANITY=[0|1]</td>
		<td class="option-description">If this option is set a sanity check will be performed for every benchmark executed. The default value is 0.</td>
	</tr>
	<tr>
		<td class="option-name">ITERATIONS=&lt;IntegerNumber&gt;</td>
		<td class="option-description">This option controls how many times each benchmark will be executed. It must be a positive integer number. The default value is 1.</td>
	</tr>
	<tr>
		<td class="option-name">NUMBER_OF_SCHEDULERS=[Num1,Num2,...|Num1..Num2]</td>
		<td class="option-description">This option defines how many schedulers will be created by Erlang in order to execute a benchmark. It can have two forms. The first form is a comma separated list of numbers. In this case, each benchmark will be executed multiple times, each time using the corresponding number of schedulers. For example, if we set NUMBER_OF_SCHEDULERS=1,2,4,8 each benchmark will be executed 4 times using 1,2,4 and 8 schedulers in each case. The second form declares a range. For example, if we set NUMBER_OF_SCHEDULERS=1..8 each benchmark will be executed 8 times using from 1 up to 8 schedulers. The default value is the number of hardware threads available on the system.</td>
	</tr>
	<tr>
		<td class="option-name">NUMBER_OF_SLAVE_NODES=[Num1,Num2,...|Num1..Num2]</td>
		<td class="option-description">This option defines how many slave nodes will be started to run the benchmarks. The two forms it accepts are the same as for the NUMBER_OF_SCHEDULERS option. The default value is 0.</td>
	</tr>
	<tr>
		<td class="option-name">VERSION=[short|intermediate|long]</td>
		<td class="option-description">Each benchmark has three pre-defined versions for executing it: short, intermediate and long. They differ in the parameters passed to the benchmark, which affect its execution time. The default value is short.</td>
	</tr>
</table>

#### <a name="Extending"></a> EXTENDING THE BENCHMARK SUITE

The benchmark suite is extensible and can be enhanced with new benchmarks, both synthetic and real-world. A few steps must be followed to add a new benchmark to the suite.

If the benchmark is written for a real-world, open-source application, one can add this application in a directory under the suite's `app/` directory.
This helps the benchmark suite to be self-contained.  It also allows the target application to be built and run with the same Erlang/OTP as the benchmark.

The first step is to create a directory for the benchmark under the directory `bench/`. This is where everything related to the new benchmark will end up.

In the new benchmark directory a few sub-directories have to be created. The first one is `src/`, where the benchmark's source code will reside.
One needs to write a handler for the benchmark, which needs to reside in that directory. A *benchmark handler* is a standard Erlang
module that has the same name with the benchmark and exports two functions: `bench_args/2` and `run/3`. Essentially, during execution of the benchmark the suite interacts with this module.

Function `bench_args/2` has the following signature:

	bench_args(Version, Conf) -> Args
		when
			Args :: [[term()]],
			Conf   :: [{Key :: atom(), Val :: term()}, ...],
			Version :: short | intermediate | long.

It returns the different argument sets that should be used to run this `Version` of the benchmark. Information from the configuration 
(`Conf`) of the benchmark can be used (e.g., the number of available cores), in order to generate an appropriate argument set for each execution environment.
For example the `orbit_int` benchmark defines `bench_args/2` as follows:

	bench_args(Version, Conf) ->
		{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
		[F1, F2, F3] = case Version of
			short -> [fun bench:g13/1, 11, 2];
			intermediate -> [fun bench:g124/1, 157, 2];
			long -> [fun bench:g1245/1, 157, 2]
		end,
		[[IWP,G,N,W] || IWP <- [true,false], G <- [F1], N <- [F2 * Cores], W <- [F3 * Cores]].

The parameters `G`, `N` and `W` are different for each size; on the other hand, the benchmark is run both with enabled and with disabled 
intra-worker parallelism (i.e., with the parameter `IWP` being first `true` and then `false`).

Function `run/3` has the following signature:

	run(Args, Slaves, Conf) -> ok | {error, Reason}
		when
			Args   :: [term()],
			Slaves :: [node()],
			Conf   :: [{Key :: atom(), Val :: term()}, ...],
			Reason :: term().

It uses the arguments in `Args`, the slave nodes in `Slaves` and the settings in `Conf` to run the benchmark.
The following is the definition of `run/3` in the `orbit_int` benchmark:

	run([true,G,N,W|_], [], _) ->
		io:format("~p~n", [apply(bench, par, [G,N,W])]);
	run([false,G,N,W|_], [], _) ->
		io:format("~p~n", [apply(bench, par_seq, [G,N,W])]);
	run([true,G,N,W|_], Slaves, _) ->
		io:format("~p~n", [apply(bench, dist, [G,N,W,Slaves])]);
	run([false,G,N,W|_], Slaves, _) ->
		io:format("~p~n", [apply(bench, dist_seq, [G,N,W,Slaves])]).

This function ignores `Conf` and uses `Args` and `Slaves`, in order to calculate the orbit and display its size.
Depending on whether intra-worker parallelism is enabled or not, and whether the benchmark is to
run on multiple nodes, the appropriate function of module `bench` is called.

Our benchmark directory can also contain a `conf/` directory. If we want to define a specific run configuration for our benchmark (that will
override the run configuration of the suite), we can create a `bench.conf` file in the `conf/` sub-directory of our benchmark directory,
and add all the settings in it. In other words, the `bench.conf` file overrides the settings of the global `run.conf` file. The `bench.conf` file
can contain two more variables that are not specified in `run.conf`. These are `EXTRA_CODE_PATH`, which points to a directory that contains more
BEAM files that are necessary for the execution of the benchmark, and `EXTRA_ERL_ARGS`, which contains more command-line arguments to pass to the `erl` program.

The `conf/` directory can also contain two more files: a `pre_bench` and a `post_bench` file.
These files serve as "hooks" that are called before and after the execution of a benchmark in a new runtime environment, respectively.
Finally, in case our benchmark needs external data, we can put them in the `data/` sub-directory of `conf/`.
