---
title: Download
index: 3
---

### DOWNLOAD THE SOURCE CODE

The latest source code for `BenchErl` is freely available on [github](https://github.com/k4t3r1n4/Bencherl).

### INSTALLATION INSTRUCTIONS

The installation of BenchErl is straight-forward, as it follows the typical Unix/Linux approach. After downloading the source code, simply enter the directory `BenchErl`
and type `make` at the command prompt:

	~ $ cd BenchErl  
	~ $ make

This will compile both, the synthetic benchmarks and the applications included in the suite. If you would like to build only the benchmarks then you should run:

	~ $ make bench

Finally, if you would like to only build the applications then you have to run:

	~ $ make app

#### SUPPORTED PLATFORMS

The benchmark suite has been tested on the following systems. It should however build and run on other Unix/Linux systems.

* Debian Linux 6.0 x86-64
	* Erlang/OTP version R14A, R14B, R14B01, R14B02, R14B03, R14B04, R15B, R15B01

* Fedora 16 x86-64
	* Erlang/OTP version R14A, R14B, R14B01, R14B02, R14B03, R14B04, R15B, R15B01

### USER MANUAL

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

