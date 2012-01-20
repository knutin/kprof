# kprof

kprof allows you to measure time spent in interesting functions on the
execution path of a request, even as it travels across processes.

kprof is lightweight and allows tracing thousands of requests per
second.

## Project state

kprof is very much in the stuff-breaks-all-the-time stage. There are
very few features apart from the most basic so I would love to get
feedback on what could make the "product" more usable! Any form of
ideas/suggestions/musings are very much welcome. I also welcome
patches and pull requests.

kprof is used by wooga (github.com/wooga, wooga.com), the third
largest social games developer in the world.

## How it works

Using the built-in trace tools in Erlang, a request is marked with a
"sequential trace token". This token is used to trace execution across
processes.

Trace messages are handled outside the target VM, for minimum
impact. kprof thus needs to run inside a separate VM that is connected
to the target node over the Erlang distribution, so kprof can manage
the tracing.

## Usage

The entry point you want traced need to be patched to set and remove a
token that is used to identify the request. kprof provides helpers to
do this:

 * kprof:do_apply(M, F, A) which manages the token and runs
   erlang:apply/3 with the supplied arguments

 * kprof:misultin_req_loop(M, F) which returns a fun suitable for use
   with misultins request loop. The fun will invoke M:F(Request).

kprof:set_token/0 and kprof:clear_token/0 are the true workhorses and
might be used inside your own code if you do not want to use the
provided helpers.

To start tracing, start kprof (kprof:start()) and call
kprof:start_trace/1 like this:

    kprof:start_trace([{tier_config, TierConfig},
                       {identity_f, IdentityF},
                       {print_calls, false},
                       {stats_dumper, {file, "stats.log"}}]).


## Tiers

The trace is broken down into "tiers", which allows for much smaller
impact due to tracing and simpler aggregation. You can think of a tier
as the function to trace and aggregate. There is no need for any
hierarchy, so you can trace "sibling" function calls.

When you start tracing, you must supply the tier configuration on the
following format:

    [{TierName::atom(), {M::atom(), F:atom(), A::integer()}}]

For example, a simple key-value system might be broken down like this:

    [{parsing, {network_server, parse_request, 1}}, % decoding network message
     {handle, {requests, handle_request, 2}},
     {formatter, {formatter, format_response, 1}},
     {db, {db, network_request, 1}}, % to measure network latency
     {accounting, {accounting, incr_reqs, 1}}]


In addition, there is a special tier "_total" that is the aggregated
data across all tiers.

## Identity

A request may be identified by a supplied function. The identity
serves as a grouper across tiers, in the case of the key-value server
above the identity might be the type of request, for example "create",
"read", "update", "delete". In other words, you will be able to know
how much time is spent in each layer for each type of request.

The identity function should be a fun of arity 1. It will receive the
module name, function name, and the actual arguments of the function
call, it should return an atom or a string. The function *must* return
'undefined' if it cannot identify the function. Example:

    fun ({requests, handle_request, [create, _]}) ->
            create;
        ({requests, handle_request, [update, _]}) ->
            update
        (_) ->
            undefined
        end

## Full trace

It is possible to trace all functions invoked during a request which
is very helpful in exploration. However, a full trace has a much
higher performance impact than tiered tracing.

Aggregation for full tracing is not possible, as it is currently done
per {Module, Function, Arity} not on the position of the call within
the call trace. This might change in the future.

## Asynchronicity

When kprof is tracing a request there is at least one case where code
executed outside the request may be (partially) traced. If any process
along the way sends a message fire-and-forget style (ie. no blocking
gen_server:call/2 or receive directly after), the code executed as
caused by that message might be interleaved with code from the
request. This may easily happen in full traces. A work-around is to
trace only on tiers that makes sense.

## Code-reload

If a module with a traced function is reloaded, the trace BIFs no
longer traces that function. In this case, the trace has to be stopped
and restarted.

## Overload protection

kprof currently has no overload protection. An idea is to pause
tracing if any received trace message is older than N microseconds and
resume some time later.

## TODO

### Must-haves

 * use the set_seq_token action function in matchbody instead of using
   the token server

 * correct request parser

 * logging of slow calls

 * in kprof_tracer, throw away every trace message up until the first entry point message

 * the same traced function must be allowed to be executed multiple
   times within a request

 * proper explanation of the project in one sentence

 * overload protection in kprof_server, pause tracing, look at
   timestamp of trace messages, may be no longer than N microseconds
   in the past, configurable, resume tracing N seconds later, circuit
   breaker

 * sampling, either based on user input or frequency

 * measure impact of tracing in sample app so everybody can reproduce
   the same results

 * send trace messages to an external node: dbg:tracer(port,
   dbg:trace_port(ip, PortNumber), dbg:trace_client(ip, {HostName,
   PortNumber}, HandlerSpec).

### Nice-to-have

 * easy way to enable/disable tracing without stopping/starting the
   app, implement in the token server?

 * output trace messages to port, scripts to start/stop a "kprof node"
   where request processing and aggregation is done

 * plain text output of aggregation

 * github wiki pages detailing usage, design

 * proper/quickcheck tests of the tracer

### Wishlist

 * include information about sampling in the couchapp

 * trace garbage collection more meaningfully, send as separate call to aggregator?

 * use basho_bench to run the sample app, while profiling, make sure
   concurrency doesn't break the profiler

## Couchapp TODO

 * select specific node or all nodes

 * select time period, last 30 minutise, 60 minutes, 3 hours, etc, use for all charts
