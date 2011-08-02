# kprof

kprof allows you to profile the path a request takes from entry point
all the way through your code, even across processes.

The trace is broken down into "tiers", so you can see exactly how much
time is spent in the different layers of your system. The VM traces
only these tiers, which makes the performance impact much lower. A
full trace is also possible.

kprof is lightweight and allows tracing thousands of requests per
second. The results are aggregated on the fly, to save space and make
statistical analysis easier.

The entry points you want traced need to be patched to set and remove
a token that is used to identify the request. kprof provides helpers
to do this, for custom code and for popular webservers.

## Usage

## Tiers


## Code-reload

If a module with a traced function is reloaded, the trace BIFs no
longer traces that function. In this case, the trace has to be stopped
and restarted.

## Full trace

It is possible to trace all functions invoked during a request which
is very helpful in exploration. However, a full trace has a much
higher performance impact than tiered tracing.

Aggregation is done per {Module, Function, Arity} not on the position
of the call within the call trace. This might change in the future.

## Identity

## Overload protection

To avoid overload of the process receiving trace messages, it will
drop messages that it considers to be old. Each trace message is
timestamped by the VM and if a message older than 1(this number is
pulled out of thin air) second is encountered, tracing is aborted.

## TODO
 * overload protection in kprof_server, disable tracing, look at timestamp of trace messages
 * sampling based on frequency, sample 10% of all calls
 * full tracing of all calls in a seq trace (fabian), "slow calls" in rpm
 * run the aggregation externally to the node using {tracer, Port}
 * count number of requests to the token server
 * should be able to specify tiers using match specs
 * stop tracing gc when a request is finished
