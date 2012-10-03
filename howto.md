---
title: HOWTO
index: 3
---

#### HOW TO DOWNLOAD IT

The latest source code of BenchErl is freely available on
[github](https://github.com/softlab-ntua/bencherl).

#### HOW TO BUILD IT

In order to build BenchErl, run the following commands:

~~~~~{.bash}
$ cd bencherl
$ make
~~~~~

If you need to clean up from any previous builds, run the following command 
first:

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

#### HOW TO USE IT

Execute the <span class="code">bencherl</span> script:

~~~~~{.bash}
$ cd bencherl
$ ./bencherl
~~~~~

The <span class="code">bencherl</span> script has the following options:

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
                <td class="option-description">Use <span class="code">MNEMONIC</span> as the mnemonic name of this run (otherwise the current date and time will be used to construct the mnemonic name of this run).</td>
        </tr>
</table>

#### HOW TO CONFIGURE IT

In order to specify what you want to run and how you want to run it, you can use
the <span class="code">conf/run.conf</span> file (which is essentially a BASH 
script). Below you may find information about all the available variables that 
you may set in this particular file.

<table border="0" cellpadding="5">
        <tr>
                <td class="configuration-name">CHECK_SANITY=[0|1]</td>
                <td class="configuration-description">If set to <span class="code">1</span>, a sanity check will be performed on the results that each benchmark produced during its execution.</td>      
		</tr>
        <tr>
                <td class="configuration-name">COOKIE=&lt;Cookie&gt;</td>
                <td class="configuration-description">The cookie that will be set on all Erlang nodes that will be used for running the benchmark. The default cookie is <span class="code">cookie</span>.<
/td>
        </tr>
         <tr>
                <td class="configuration-name">ERL_ARGS=&lt;Alias1=Args1,Alias2=Args2,...&gt;</td>
                <td class="configuration-description">A comma-separated list of command-line argument sets to pass to the <span class="code">erl</span> program. An alias must be specified for each argument set. The default value is <span class="code">DEF_ARGS=</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">EXCLUDE_BENCH=&lt;Bench1,Bench2,...&gt;</td>
                <td class="configuration-description">A comma-separated list of the benchmarks that you do not want to run. By default, no benchmark is excluded.</td>
        </tr>
        <tr>
                <td class="configuration-name">INCLUDE_BENCH=&lt;Bench1,Bench2,...&gt;</td>
                <td class="configuration-description">A comma-separated list of the benchmarks that you want to run. By default, all benchmarks are executed.</td>
        </tr>
        <tr>
                <td class="configuration-name">ITERATIONS=&lt;Num&gt;</td>
                <td class="configuration-description">A positive integer that controls how many times the execution of a benchmark in a specific runtime environment will be repeated. The default value is <span class="code">1</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">MASTER_NODE=&lt;Name&gt;</td>
                <td class="configuration-description">The long or the short name for the master node. The default long name is <span class="code">master@\`hostname -f\`</span>, whereas the default short name is <span class="code">master@\`hostname\`</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">NUMBER_OF_SCHEDULERS=[Num1,Num2,...|Num1..Num2]</td>
                <td class="configuration-description">How many schedulers to use for running each benchmark. The value can be either a comma-separated list of integers or a range of integers. The default value is the number of the CPU cores of the system.</td>
        </tr>
        <tr>
                <td class="configuration-name">NUMBER_OF_SLAVE_NODES=[Num1,Num2,...|Num1..Num2]</td>
                <td class="configuration-description">How many slave nodes to use for running each benchmark. The value can be either a comma-separated list of integers or a range
 of integers. The default value is <span class="code">0</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">OTPS=&lt;Alias1=Path1,Alias2=Path2,...&gt;</td>
                <td class="configuration-description">A comma-separated list of Erlang/OTP versions to run the benchmarks with. For each Erlang/OTP version, you must specify a unique alias and the path that leads to it. The default value is <span class="code">DEF_OTP=</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">PLOT=[0|1]</td>
                <td class="configuration-description">If set to <span class="code">1</span>, time and speedup diagrams will be produced. The default value is <span class="code">1</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">SLAVE_NODES=&lt;Name1,Name2,...&gt;</td>
                <td class="configuration-description">A comma-separated list of the long or the short names of the slave nodes that participate in the execution of the benchmarks.</td>
        </tr>
        <tr>
                <td class="configuration-name">USE_LONG_NAMES=[0|1]</td>
                <td class="configuration-description">If set to <span class="code">1</span>, [long node names](http://www.erlang.org/doc/reference_manual/distributed.html#id82803) will be used. The default value is <span class="code">1</span>.</td>
        </tr>
        <tr>
                <td class="configuration-name">VERSION=[short|intermediate|long]</td>
                <td class="configuration-description">Which version of the benchmarks to run. The default value is <span class="code">short</span>.</td>
        </tr>
</table>

#### HOW TO EXTEND IT

BenchErl can be enhanced with new benchmarks, both synthetic and real-world.

If the new benchmark is written for a real-world, open-source application, then add
this application in a directory under the <span class="code">app/</span> directory.

Create a directory for the new benchmark under the <span class="code">bench/</span> directory.

In the benchmark directory, create an <span class="code">src/</span> directory. This is where the
new benchmark handler must reside. A benchmark handler is a standard Erlang
module that has the same name with the benchmark and exports the following
functions:

~~~~~{.erlang}
%% Returns the arguments to use for running the specified version of the
%% benchmark under the specified configuration settings.
bench_args(Version, Conf) -> Args
    when
        Version :: short | intermediate | long.
        Conf   :: [{Key :: atom(), Val :: term()}, ...],
        Args :: [[term()]],
~~~~~

~~~~~{.erlang}
%% Runs the benchmark using the specified arguments, the specified slave nodes
%% and the specified configuration settings.
run(Args, Slaves, Conf) -> ok | {error, Reason}
    when
        Args   :: [term()],
        Slaves :: [node()],
        Conf   :: [{Key :: atom(), Val :: term()}, ...],
        Reason :: term().
~~~~~

The benchmark directory can also contain a <span class="code">conf/</span> directory. If you want to
specify different configuration settings for the benchmark (that will override
those of the suite), then create a <span class="code">bench.conf</span> file in this directory. Only the following variables can be overriden by the benchmarks:

* <span class="code">CHECK_SANITY</span>
* <span class="code">COOKIE</span>
* <span class="code">ERL_ARGS</span>
* <span class="code">ITERATIONS</span>
* <span class="code">NUMBER_OF_SLAVE_NODES</span>
* <span class="code">NUMBER_OF_SCHEDULERS</span>
* <span class="code">OTPS</span>
* <span class="code">PLOT</span>
* <span class="code">SLAVE_NODES</span>  

If you want to perform any
actions before or after the execution of the benchmark, then create a
<span class="code">pre_bench</span> or a <span class="code">post_bench</span> file, respectively, in the benchmark's <span class="code">conf</span> directory.

If the benchmark needs any external data, create a <span class="code">data/</span> directory in the
benchmark directory and put them in there.
