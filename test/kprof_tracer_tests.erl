-module(kprof_tracer_tests).

-include_lib("eunit/include/eunit.hrl").

simple_call_test() ->
    {Calls, Acc} = kprof_tracer:take_request(simple_trace_messages()),
    ?assertEqual([call, call, return_from, return_from], get_types(Calls)),
    ?assertEqual([], Acc).

no_return_test() ->
    {Calls, Acc} = kprof_tracer:take_request(no_return_messages()),
    ?assertEqual([call, call, return_from], get_types(Calls)),
    ?assertEqual([], Acc).


interleaved_test() ->
    {[Calls], Acc} = kprof_tracer:process_messages(interleaved_trace_messages()),
    ?assertEqual([{sample_app, handle_op, 4},
                  {sample_app, storage_handle_op, 1}], get_mfa(Calls)),
    ?assert(length(Acc) =:= 2).

split_test() ->
    {_First, Second} = split_trace_messages(),
    {Calls, _} = kprof_tracer:take_request(Second),
    ?assertEqual([], Calls).

    
simple_trace_messages() ->
    Client = client,
    Server = server,
    [
     {trace_ts,Client,call,
      {sample_app,handle_op,[Client,get,0,0]},
      {0,5000,0,Client,0},
      {1312,628093,391861}},

     {trace_ts,Server,call,
      {sample_app,storage_handle_op,[{Client,get,0,0}]},
      {0,5000,1,Client,0},
      {1312,628093,391900}},

     {trace_ts,Server,return_from,
      {sample_app,storage_handle_op,1},
      ok,
      {1312,628093,393007}},

     {trace_ts,Client,return_from,
      {sample_app,handle_op,4},
      done,
      {1312,628093,393017}}].

split_trace_messages() ->
    Client = client,
    Server = server,
    {[
      {trace_ts,Client,call,
       {sample_app,handle_op,[Client,get,0,0]},
       {0,5000,0,Client,0},
       {1312,628093,391861}},

      {trace_ts,Server,call,
       {sample_app,storage_handle_op,[{Client,get,0,0}]},
       {0,5000,1,Client,0},
       {1312,628093,391900}}],

     [{trace_ts,Server,return_from,
       {sample_app,storage_handle_op,1},
       ok,
       {1312,628093,393007}},

      {trace_ts,Client,return_from,
       {sample_app,handle_op,4},
       done,
       {1312,628093,393017}}]}.

no_return_messages() ->
    Client = client,
    Server = server,
    [
     {trace_ts,Client,call,
      {sample_app,handle_op,[Client,get,0,0]},
      {0,5000,0,Client,0},
      {1312,628093,391861}},

     {trace_ts,Server,call,
      {sample_app,storage_handle_op,[{Client,get,0,0}]},
      {0,5000,1,Client,0},
      {1312,628093,391900}},
     
     {trace_ts,Client,return_from,
      {sample_app,handle_op,4},
      done,
      {1312,628093,393017}}].

interleaved_trace_messages() ->
    Client = client,
    Server = server,
    Server2 = server2,

    [
     {trace_ts,Client,call,
      {sample_app,handle_op,[Client,get,0,0]},
      {0,5000,0,Client,0},
      {1312,628093,391861}},

     {trace_ts,Server,call,
      {sample_app,storage_handle_op,[{Client,get,0,0}]},
      {0,5000,1,Client,0},
      {1312,628093,391900}},

     {trace_ts,Server,return_from,
      {sample_app,storage_handle_op,1},
      ok,
      {1312,628093,393007}},

     {trace_ts,Server2,call,
      {sample_app,storage_handle_op,[{Client,get,1,1}]},
      {0,5001,1,Client,0},
      {1312,628093,391900}},

     {trace_ts,Server2,return_from,
      {sample_app,storage_handle_op,1},
      ok,
      {1312,628093,393007}},

     {trace_ts,Client,return_from,
      {sample_app,handle_op,4},
      done,
      {1312,628093,393017}}].    


get_types(Messages) ->    
    lists:map(fun ([]) ->
                      no_return;
                  (T) ->
                      element(3, T)
              end, Messages).

get_mfa(Calls) ->
    lists:map(fun ({MFA, _, _, _, _}) -> MFA end, Calls).
                               

