---
title: Benchmarks
index: 1
---

At the moment, BenchErl consists of **14** benchmarks, which correspond to both [**synthetic**](#synthetic_benchmarks) and [**real-world**](#real_world) use cases.

We welcome additions to this set; see the instructions on <a href="howto.html#extend">how to extend</a> BenchErl for more information on how to add more benchmarks to it.

#### <a name="synthetic_benchmarks"></a> SYNTHETIC BENCHMARKS

<table border="0" cellpadding="5">
	<tr>
		<td class="bench-name">bang</td>
		<td class="bench-description">
			A benchmark for many-to-one message passing that spawns one receiver
			and multiple senders that flood the receiver with messages. The 
			benchmark is parameterized by the number of senders to spawn and the
			number of messages that each sender sends to the receiver.
		</td>
	</tr>
	<tr>
		<td class="bench-name">big</td>
		<td class="bench-description">
			A benchmark that implements a many-to-many message passing scenario.
			Several processes are spawned, each of which sends a *ping* message
			to the others, and responds with a *pong* message to any *ping* 
			message it receives. The benchmark is parameterized by the number of
			processes.
		</td>
	</tr>
	<tr>
		<td class="bench-name">ehb</td>
		<td class="bench-description">
			This is an implementation of *hackbench* in Erlang, a benchmark and
			stress test for Linux schedulers. The number of groups and the 
			number of messages that each sender sends to each receiver in the 
			same group are the two parameters that this benchmark receives.
		</td>
	</tr>
	<tr>
		<td class="bench-name">ets_test</td>
		<td class="bench-description">
			This benchmark creates an ETS table and spawns several readers and 
			writers that perform a certain number of reads (lookups) and writes 
			(inserts), respectively, to that table. The benchmark is 
			parameterized by the number of readers, the number of writers and 
			the number of operations (inserts/lookups) that each reader or 
			writer performs.
		</td>
	</tr>
	<tr>
		<td class="bench-name">genstress</td>
		<td class="bench-description">
			This is a generic server benchmark that spawns an echo server and a 
			number of clients. Each client fills its message queue with a number
			of dummy messages; it then sends some messages to the echo server 
			and waits for its response. The benchmark can be executed with or 
			without using the <span class="code">gen_server</span> behaviour, as			well as with a different number of clients, dummy messages and 
			messages exchanged with the echo server.
		</td>
	</tr>
	<tr>
		<td class="bench-name">mbrot</td>
		<td class="bench-description">
			This benchmark extrapolates the coordinates of a 2-D complex plane 
			that correspond to the pixels of a 2-D image of a specific 
			resolution. For each one of these points, the benchmark determines 
			whether the point belongs to the *Mandelbrot set* or not. The total 
			set of points is divided among a number of workers. The benchmark is			parameterized by the dimensions of the image.
		</td>
	</tr>
	<tr>
		<td class="bench-name">orbit_int</td>
		<td class="bench-description">
			The *orbit problem* is defined as follows: Given a space $X$, a list			of generators $f_1,...,f_n : X \rightarrow X$ and an initial vertex
			$x_0 \in X$, compute the least subset $\mathit{Orb} \subseteq X$, 
			such that $x_0 \in \mathit{Orb}$ and $\mathit{Orb}$ is closed under 
			all generators. We consider a special case of the orbit problem, 
			where $X$ is a finite subset of the natural numbers. This benchmark
			operates on a distributed hash table, and follows a master/worker 
			architecture. The master initiates the computation, and waits for 
			its termination. Each worker hosts a chunk of the hash 
			table. When a worker receives a vertex, it stores it into its chunk,
			applies all generators to it, and then sends the generated vertices 
			to the corresponding nodes that are responsible for them. The master
			and all the workers are processes on the same or on different Erlang 			nodes. The benchmark creates the hash table, distributes it evenly 
			across the workers, and computes the orbit in parallel. The 
			parameters of the benchmark are a list of generators, the size of 
			the space, the number of workers, a list of nodes to spawn workers 
			on, and whether there will be intra-worker parallelism or not.
		</td>
	</tr>
	<tr>
		<td class="bench-name">parallel</td>
		<td class="bench-description">
			A benchmark for parallel execution that spawns a number of 
			processes, each of which creates a list of $N$ timestamps and, after
			it checks that each element of the list is strictly greater than its
			previous one (as promised by the implementation of 
			<span class="code">erlang:now/0</span>), it sends the result to its 
			parent. The benchmark is parameterized by the number of processes 
			and the number of timestamps.
		</td>
	</tr>
	<tr>
		<td class="bench-name">pcmark</td>
		<td class="bench-description">
			This benchmark is also about ETS operations. It creates five ETS 
			tables, fills them with values, and then spawns a certain number of 
			processes that read the contents of those tables and update them. As
			soon as one process finishes, a new process is spawned, until a 
			certain total number of processes has been reached. The benchmark is
			parameterized by the number of initial processes and the total 
			number of processes.
		</td>
	</tr>
	<tr>
		<td class="bench-name">ran</td>
		<td class="bench-description">
			Another benchmark for parallel execution that spawns a certain 
			number of processes, each of which generates a list of ten thousand
			random integers, sorts it and sends its first half to the parent 
			process. The benchmark receives the number of processes as a 
			parameter.
		</td>
	</tr>
	<tr>
		<td class="bench-name">serialmsg</td>
		<td class="bench-description">
			A benchmark about message proxying through a dispatcher. The 
			benchmark spawns a certain number of receivers, one dispatcher, and 
			a certain number of generators. The dispatcher forwards the messages
			that it receives from generators to the appropriate receiver. Each 
			generator sends a number of messages to a specific receiver. The 
			parameters of the benchmark are the number of receivers, the number
			of messages and the message length.
		</td>
	</tr>
	<tr>
		<td class="bench-name">timer_wheel</td>
		<td class="bench-description">
			A timer management benchmark that spawns a certain number of 
			processes that exchange *ping* and *pong* messages. Each process 
			sends a *ping* message to all other processes, and then waits (with
			or without a timeout) to receive a *pong* message as a response. In
			the meantime, the process responds with a *pong* message to any 
			*ping* message it receives. In case of a timeout, the corresponding 
			process dies. The benchmark is parameterized by the number of 
			processes.
		</td>
	</tr>
</table>

#### <a name="real_world"></a> REAL-WORLD BENCHMARKS

<table border="0" cellpadding="5">
	<tr>
		<td class="bench-name">dialyzer_bench</td>
		<td class="bench-description">
			[Dialyzer](http://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html) 
			is a static analysis tool that identifies software discrepancies 
			(e.g. definite type errors, unreachable code, redundant tests) in 
			single Erlang modules or entire applications. The benchmark 
			generates a Persistent Lookup Table (PLT) for the most common 
			Erlang/OTP applications, and then uses this PLT to analyze all the 
			major applications that are included in the Erlang/OTP distribution.
		</td>
	</tr>
	<tr>
		<td class="bench-name">scalaris_bench</td>
		<td class="bench-description">
			[Scalaris](http://code.google.com/p/scalaris) is an Erlang 
			implementation of a distributed key-value store, which has been 
			designed for good horizontal scalability, i.e., good performance for
			simple read/write operations distributed over many servers. The 
			benchmark creates a ring with a certain number of Scalaris nodes, 
			and spawns a certain number of processes on each one of them. Each 
			process picks a random key, and reads its value a certain number of
			times.
		</td>
	</tr>
</table>

