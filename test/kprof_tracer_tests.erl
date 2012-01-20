-module(kprof_tracer_tests).

-include_lib("eunit/include/eunit.hrl").

-record(state, {traces, returns, entrypoint, request_target}).

handler_test() ->
    [M1, M2, M3, M4] = simple_trace_messages(),

    {Tracer, S0} = kprof_tracer:mk_tracer(entry(), self()),

    S1 = Tracer(M1, S0),
    S2 = Tracer(M2, S1),
    S3 = Tracer(M3, S2),
    S4 = Tracer(M4, S3),

    ?assertEqual(dict:new(), S4#state.traces),
    ?assertEqual(dict:new(), S4#state.returns),
    ?assertEqual(simple_trace_messages(), receive {trace, Msgs} -> Msgs end),

    ok.

%% simple_call_test() ->
%%     {Calls, Acc} = kprof_tracer:take_request(entry_point(), simple_trace_messages()),
%%     ?assertEqual([call, call, return_from, return_from], get_types(Calls)),
%%     ?assertEqual([], Acc).

%% no_return_test() ->
%%     ?assertThrow({kprof, missing_return, _},
%%                  kprof_tracer:take_request(entry_point(), no_return_messages())).

%% interleaved_test() ->
%%     {[Calls], Acc} = kprof_tracer:process_messages(entry_point(), interleaved_trace_messages()),
%%     ?assertEqual([{sample_app, handle_op, 4},
%%                   {sample_app, storage_handle_op, 1}], get_mfa(Calls)),
%%     ?assert(length(Acc) =:= 2).

%% split_test() ->
%%     {_First, Second} = split_trace_messages(),
%%     {Calls, _} = kprof_tracer:take_request(entry_point(), Second),
%%     ?assertEqual([], Calls).


%% misultin_test() ->
%%     {[Call], _Acc} = kprof_tracer:process_messages({misultin_http, handle_data, 9},
%%                                                    misultin_req()),
%%     ExpectedMFA = [{misultin_http,handle_data,9},
%%                    {misultin_http,headers,3},
%%                    {misultin_http,read_post_body,2},
%%                    {misultin_http,call_mfa,2},
%%                    {webserver,handle_http,1}],
%%     ?assertEqual(ExpectedMFA, get_mfa(Call)).

%% misultin_socket_loop_test() ->
%%     {[Call], _Acc} = kprof_tracer:process_messages({misultin_http, handle_data, 9},
%%                                                    misultin_socket_loop()),
%%     ExpectedMFA = [{misultin_http,handle_data,9},
%%                    {misultin_http,headers,3},
%%                    {misultin_http,read_post_body,2},
%%                    {misultin_http,call_mfa,2},
%%                    {misultin_http,socket_loop,4},
%%                    {webserver,handle_http,1},
%%                    {foo_module,call,2},
%%                    {misultin_http,socket_loop,4}
%%                   ],
%%     ?assertEqual(ExpectedMFA, get_mfa(Call)).


%%
%% HELPERS AND DATA
%%

entry() ->
    {sample_app, handle_op, 4}.

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


misultin_req() ->
    AcceptorPid = spawn(fun() -> ok end),
    MisultinPid = spawn(fun() -> ok end),
    WorkerPid = spawn(fun() -> ok end),

    [{trace_ts,MisultinPid,call,
           {misultin_http,handle_data,
               [AcceptorPid,port,http,8080,
                {127,0,0,1},
                46585,undefined,30000,
                {custom_opts,4096576,2000,true,undefined,
                    fun() -> ok end,true,none,true}]},
           {0,5000,0,MisultinPid,0},
           {1318,509232,188160}},
       {trace_ts,MisultinPid,call,
           {misultin_http,headers,
               [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
                   fun() -> ok end,true,none,true},
                {req,port,http,
                    {127,0,0,1},
                    46585,undefined,keep_alive,undefined,
                    {1,1},
                    'POST',
                    {abs_path,"/foo"},
                    [],undefined,<<>>},
                []]},
           {0,5000,1,MisultinPid,0},
           {1318,509232,188293}},
       {trace_ts,MisultinPid,call,
           {misultin_http,read_post_body,
               [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
                   fun () -> ok end,true,none,true},
                {req,port,http,
                    {127,0,0,1},
                    46585,undefined,keep_alive,"53",
                    {1,1},
                    'POST',
                    {abs_path,"/foo"},
                    [],
                    [],
                    <<>>}]},
           {0,5000,1,MisultinPid,0},
           {1318,509232,188329}},
       {trace_ts,MisultinPid,return_from,
           {misultin_http,read_post_body,2},
           {ok,<<"foo">>},
           {1318,509232,188348}},
       {trace_ts,MisultinPid,call,
           {misultin_http,call_mfa,
               [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
                   fun () -> ok end,true,none,true},
                {req,port,http,
                    {127,0,0,1},
                    46585,undefined,keep_alive,"53",
                    {1,1},
                    'POST',
                    {abs_path,"/foo"},
                    [],
                    [],
                    <<>>}]},
           {0,5000,1,MisultinPid,0},
           {1318,509232,188352}},
       {trace_ts,WorkerPid,call,
           {webserver,handle_http,
               [{misultin_req,
                    {req,port,http,
                        {127,0,0,1},
                        46585,undefined,keep_alive,"53",
                        {1,1},
                        'POST',
                        {abs_path,"/foo"},
                        [],
                        [],
                        <<>>},
                    MisultinPid}]},
           {0,5000,0,WorkerPid,0},
           {1318,509232,188400}},
       {trace_ts,WorkerPid,return_from,
           {webserver,handle_http,1},
           {response,200,
               [],
               <<"foo">>},
           {1318,509232,491753}},
       {trace_ts,MisultinPid,return_from,
           {misultin_http,call_mfa,2},
           true,
           {1318,509232,491844}},
       {trace_ts,MisultinPid,return_from,
           {misultin_http,headers,3},

           ok,
           {1318,509232,493103}},
       {trace_ts,MisultinPid,return_from,
           {misultin_http,handle_data,9},
           ok,
           {1318,509232,493108}}].

misultin_socket_loop() ->
    AcceptorPid = spawn(fun() -> ok end),
    MisultinPid = spawn(fun() -> ok end),
    WorkerPid = spawn(fun() -> ok end),

    [{trace_ts,MisultinPid,call,
      {misultin_http,handle_data,
       [AcceptorPid,port,http,8080,
        {127,0,0,1},
        46692,undefined,30000,
        {custom_opts,4096576,2000,true,undefined,
         fun () -> ok end,true,none,true}]},
      {0,5351,0,MisultinPid,0},
      {1318,848023,229387}},
     {trace_ts,MisultinPid,call,
      {misultin_http,headers,
       [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
         fun () -> ok end,true,none,true},
        {req,port,http,
         {127,0,0,1},
         46692,undefined,keep_alive,undefined,
         {1,1},
         'POST',
         {abs_path,"/foo"},
         [],undefined,<<>>},
        []]},
      {0,5351,1,MisultinPid,0},
      {1318,848023,229437}},
     {trace_ts,MisultinPid,call,
      {misultin_http,read_post_body,
       [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
         fun () -> ok end,true,none,true},
        {req,port,http,
         {127,0,0,1},
         46692,undefined,keep_alive,"53",
         {1,1},
         'POST',
         {abs_path,"/foo"},
         [],
         [],
         <<>>}]},
       {0,5351,1,MisultinPid,0},
       {1318,848023,229485}},
      {trace_ts,MisultinPid,return_from,
       {misultin_http,read_post_body,2},
       {ok,<<>>},
       {1318,848023,229513}},
      {trace_ts,MisultinPid,call,
       {misultin_http,call_mfa,
        [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
          fun () -> ok end,true,none,true},
         {req,port,http,
          {127,0,0,1},
          46692,undefined,keep_alive,"53",
          {1,1},
          'POST',
          {abs_path,"/foo"},
          [],
          [],
          <<>>}]},
        {0,5351,1,MisultinPid,0},
        {1318,848023,229519}},
       {trace_ts,MisultinPid,call,
        {misultin_http,socket_loop,
         [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
           fun () -> ok end,true,none,true},
          {req,port,http,
           {127,0,0,1},
           46692,undefined,keep_alive,"53",
           {1,1},
           'POST',
           {abs_path,"/users/124/setup"},
           [],
           [],
           <<>>},
           MisultinPid,
           {req_options,false}]},
         {0,5351,1,MisultinPid,0},
         {1318,848023,229535}},
        {trace_ts,WorkerPid,call,
         {webserver,handle_http,
          [{misultin_req,
            {req,port,http,
             {127,0,0,1},
             46692,undefined,keep_alive,"53",
             {1,1},
             'POST',
             {abs_path,"/foo"},
             [],
             [],
             <<>>},
             MisultinPid}]},
          {0,5351,0,WorkerPid,0},
          {1318,848023,229557}},
         {trace_ts,WorkerPid,call,
          {foo_module,call,
           ["foo",
            {foo,bar,baz}]},
          {0,5351,0,WorkerPid,0},
          {1318,848023,229599}},
         {trace_ts,WorkerPid,return_from,
          {foo_module,call,2},
          foo_return,
          {1318,848023,230334}},
         {trace_ts,WorkerPid,return_from,
          {webserver,handle_http,1},
          {response,200,
           [],
           <<>>},
          {1318,848023,230463}},
         {trace_ts,MisultinPid,call,
          {misultin_http,socket_loop,
           [{c,AcceptorPid,port,http,8080,30000,4096576,2000,true,
             fun () -> ok end,true,none,true},
            {req,port,http,
             {127,0,0,1},
             46692,undefined,keep_alive,"53",
             {1,1},
             'POST',
             {abs_path,"/foo"},
             [],
             [],
             <<>>},
            WorkerPid,
            {req_options,false}]},
          {0,5351,249832,WorkerPid,249830},
          {1318,848023,230632}},
         {trace_ts,MisultinPid,return_from,
          {misultin_http,socket_loop,4},
          ok,
          {1318,848023,230637}},
         {trace_ts,MisultinPid,return_from,
          {misultin_http,socket_loop,4},
          ok,
          {1318,848023,230638}},
         {trace_ts,MisultinPid,return_from,
          {misultin_http,call_mfa,2},
          true,
          {1318,848023,230641}},
         {trace_ts,MisultinPid,return_from,
          {misultin_http,headers,3},
          ok,
          {1318,848023,230886}},
         {trace_ts,MisultinPid,return_from,
          {misultin_http,handle_data,9},
          ok,
          {1318,848023,230891}}].
