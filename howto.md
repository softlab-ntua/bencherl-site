---
title: How to
index: 3
---

### Download

The latest source code for `BenchErl` is freely available on [github](https://github.com/k4t3r1n4/Bencherl).

### Build

Run the following commands to build `BenchErl`:

~~~~~{.bash}
$ cd bencherl
$ make
~~~~~

If you need to clean up from previous builds before the new build, run the following command first:

~~~~~{.bash}
$ make clean 
~~~~~

If you want to build only the benchmarks, run the following command:

~~~~~{.bash}
$ make bench
~~~~~

If you want to build only the applications, run the following command:

~~~~~{.bash}
$ make app
~~~~~

### Use

Run `bencherl`:

~~~~~{.bash}	
$ cd bencherl
$ ./bencherl  
~~~~~

`bencherl` has the following options:

<table border="0" cellpadding="5">
	<tr>
		<td class="option-name">-h</td>
		<td class="option-description">Display a short help message and exit.</td>
	</tr>
	<tr>
		<td class="option-name">-l</td>
		<td class="option-description">List all the available benchmarks.</td>
	</tr>
	<tr>
		<td class="option-name">-m &lt;MNEMONIC&gt;</td>
		<td class="option-description">Use `MNEMONIC` as the mnemonic name of this run (otherwise the current date and time will be used to construct the mnemonic name of this run).</td>
	</tr>
</table>

#### Configure

In order to specify what you want to run and how you want to run it, you can use the `conf/run.conf` file (which is essentially a BASH script). Below you may find information about all the available configuration settings.

<table border="0" cellpadding="5">
	<tr>
		<td class="configuration-name">USE_LONG_NAMES=[0|1]</td>
		<td class="configuration-description">If set to `1`, [long node names](http://www.erlang.org/doc/reference_manual/distributed.html#id82803) will be used. The default value is `1`.</td>
	</tr>
	<tr>
		<td class="configuration-name">MASTER_NODE=&lt;Name&gt;</td>
		<td class="configuration-description">A long or short name for the master node. The default value is `master`.</td>
	</tr>
	<tr>
		<td class="configuration-name">SLAVE_NODES=&lt;Name1,Name2,...&gt;</td>
		<td class="configuration-description">A comma-separated list of long or short names for the slave nodes.</td>
	</tr>
	<tr>
		<td class="configuration-name">OTPS=&lt;Alias1=Path1,Alias2=Path2,...&gt;</td>
		<td class="configuration-description">A comma-separated list of Erlang/OTP versions to run the benchmarks with. For each Erlang/OTP version, you must specify a unique alias and the path that leads to it. The default value is `DEF_OTP=`.</td>
	</tr>
	<tr>
		<td class="configuration-name">ERL_ARGS=&lt;Alias1=Args1,Alias2=Args2,...&gt;</td>
		<td class="configuration-description">A comma-separated list of command-line argument sets to pass to the `erl` program. An alias must be specified for each argument set. The default value is `DEF_ARGS=`.</td>
	</tr>
	<tr>
		<td class="configuration-name">INCLUDE_BENCH=&lt;Bench1,Bench2,...&gt;</td>
		<td class="configuration-description">A comma-separated list of the benchmarks that you want to run. By default, all benchmarks are executed.</td>
	</tr>
	<tr>
		<td class="configuration-name">EXCLUDE_BENCH=&lt;Bench1,Bench2,...&gt;</td>
		<td class="configuration-description">A comma-separated list of the benchmarks that you do not want to run. By default, no benchmark is excluded.</td>
	</tr>
	<tr>
		<td class="configuration-name">COOKIE=&lt;Cookie&gt;</td>
		<td class="configuration-description">The cookie that will be set on all Erlang nodes that will be used for running the benchmark. The default cookie is `cookie`.</td>
	</tr>
	<tr>
		<td class="configuration-name">PLOT=[0|1]</td>
		<td class="configuration-description">If set to `1`, time and speedup diagrams will be produced. The default value is `1`.</td>
	</tr>
	<tr>
		<td class="configuration-name">CHECK_SANITY=[0|1]</td>
		<td class="configuration-description">If set to `1`, a sanity check will be performed on the results that each benchmark produced during its execution.</td>
	</tr>
	<tr>
		<td class="configuration-name">ITERATIONS=&lt;Num&gt;</td>
		<td class="configuration-description">A positive integral number that controls how many times each execution of a benchmark will be repeated. The default value is `1`.</td>
	</tr>
	<tr>
		<td class="configuration-name">NUMBER_OF_SCHEDULERS=[Num1,Num2,...|Num1..Num2]</td>
		<td class="configuration-description">How many schedulers to use for running each benchmark. The value can be either a comma-separated list of integers or a range of integers. The default value is the number of the CPU cores of the system.</td>
	</tr>
	<tr>
		<td class="configuration-name">NUMBER_OF_SLAVE_NODES=[Num1,Num2,...|Num1..Num2]</td>
		<td class="configuration-description">How many slave nodes to use for running each benchmark. The value can be either a comma-separated list of integers or a range od integers. The default value is `0`.</td>
	</tr>
	<tr>
		<td class="configuration-name">VERSION=[short|intermediate|long]</td>
		<td class="configuration-description">Which version of the benchmarks to run. The default value is `long`.</td>
	</tr>
</table>

#### Extend

`BenchErl` can be enhanced with new benchmarks, both synthetic and real-world.

If the benchmark is written for a real-world, open-source application, then add this application in a directory under the `app/` directory.

Create a directory for the benchmark under the `bench/` directory.

In the benchmark directory, create an `src/` directory. This is where the benchmark handler must reside. A **benchmark handler** is a standard Erlang module that has the same name with the benchmark and exports the following functions: 

~~~~~{.bash}
%% Returns the arguments to use for running the specified version of the benchmark 
%% under the specified configuration settings.
bench_args(Vrsn, Conf) -> Args
  when
    Vrsn :: short | intermediate | long.
    Conf   :: [{Key :: atom(), Val :: term()}, ...],
    Args :: [[term()]],

%% Runs the benchmark using the specified arguments, the specified slave nodes
%% and the specified configuration settings.
run(Args, Slaves, Conf) -> ok | {error, Reason}
  when
    Args   :: [term()],
    Slaves :: [node()],
    Conf   :: [{Key :: atom(), Val :: term()}, ...],
    Reason :: term().
~~~~~

The benchmark directory can also contain a `conf/` directory. If you want to specify different configuration settings for the benchmark (that will override those of the suite), then create a `bench.conf` file. If you want to perform any actions before or after the execution of the benchmark, then create a `pre_bench` or a `post_bench` file, respectively.
 
If the benchmark needs any external data, create a `data/` directory in the benchamrk directory.
 
