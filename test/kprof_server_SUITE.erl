-module(kprof_server_SUITE).
-compile([export_all]).

-include_lib("test_server/include/test_server.hrl").

all() ->
    [trace_port].

trace_port(_) ->
    kprof:stop(), kprof:start(),
    ?line {ok, Slave} = slave:start(list_to_atom(net_adm:localhost()), bar),

    ?line true = rpc:call(Slave, code, add_path, ["/home/knutin/git/kprof/ebin"]),
    rpc:call(Slave, code, ensure_loaded, [sample_app]),

    ?line ok = kprof:start_trace([{tier_config, tier_config()},
                                  {identity_f, identity_f()},
                                  {node, Slave},
                                  {user_tracer, self()}]),

    rpc:call(Slave, sample_app, r, [1, 1]),

    TraceMsgs = flush(),
    ?line 4 = length(TraceMsgs),
    slave:stop(Slave).


flush() ->
    flush([]).
flush(Acc) ->
    receive
        X ->
            flush(Acc ++ [X])
    after 1000 ->
            Acc
    end.


tier_config() ->
    [{client, {sample_app, handle_op, 4}},
     {storage, {sample_app, storage_handle_op, 1}}].

identity_f() ->
    fun ({sample_app, handle_op, [_, Op, _, _]}) ->
            Op;
        (_) -> undefined
    end.
