%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc App for generating trace events for development of kprof
%%
%% Implements a simple key-value server, with N "storage"
%% processes a key is hashed over to test concurrency

-module(sample_app).
-compile([export_all]).

%% @doc: Starts tracing, dump to couchdb on localhost
t() ->
    kprof:stop(),
    kprof:start(),

    TierConfig = [{client, {?MODULE, handle_op, 4}},
                  {storage, {?MODULE, storage_handle_op, 1}}],

    IdentityF = fun ({?MODULE, handle_op, [_, Op, _, _]}) ->
                        Op;
                    (_) -> undefined
                end,

    kprof:start_trace([{tier_config, TierConfig},
                       {identity_f, IdentityF},
                       {print_calls, false},
                       {stats_dumper, false}]).

%% @doc: Set up, run N number of requests, teardown
r(Requests, Clients) ->
    spawn(fun() ->
                  process_flag(trap_exit, true),
                  error_logger:info_msg("Starting servers~n"),
                  start_storage_processes(),

                  error_logger:info_msg("Running~n"),
                  Start = now(),

                  run_loop(Requests, 0, 0, Clients, self()),
                  End = now(),
                  ElapsedUs = timer:now_diff(End, Start),
                  error_logger:info_msg("Did ~p requests per second~n",
                            [trunc(Requests / (ElapsedUs / 1000 / 1000))])

                  %%io:format("Cleaning up~n"),
                  %%io:format("~p~n", [cleanup()])
          end).

%% @doc: Main loop, spawns workers if there is room
run_loop(MaxReq, Reqs, Clients, AllowedClients, Self)
  when Clients < AllowedClients ->
    spawn_op(Self, Reqs),
    run_loop(MaxReq, Reqs+1, Clients+1, AllowedClients, Self);
run_loop(MaxReq, Reqs, Clients, AllowedClients, Self) ->
    receive
        done ->
            case MaxReq =:= Reqs of
                true -> done;
                false ->
                    run_loop(MaxReq, Reqs, Clients-1, AllowedClients, Self)
            end;
        error ->
            run_loop(MaxReq, Reqs, Clients-1, AllowedClients, Self);
        Other ->
            io:format("received: ~p~n", [Other]),
            run_loop(MaxReq, Reqs, Clients, AllowedClients, Self)
    end.


spawn_op(Parent, N) ->
    R = trunc(random:uniform() * 5) + 1,
    Op = lists:nth(R, [get, get, get, put, get]),
    spawn(fun() ->
                  kprof:do_apply(?MODULE, handle_op, [Parent, Op, N, N])
          end).


handle_op(Client, Operation, Key, Value) ->
    Server = server_for(Key),
    case catch(erlang:send(Server, {self(), Operation, Key, Value})) of
        {_, _, _, _} ->
            receive
                ok ->
                    Client ! done
            end;
        {'EXIT', {badarg, _}} ->
            Client ! error
    end.


start_storage_processes() ->
    lists:map(fun (Name) ->
                      case whereis(Name) of
                          undefined ->
                              spawn(fun() ->
                                            register(Name, self()),
                                            storage_loop()
                                    end);
                          _ ->
                              []
                      end
              end, server_names()).


storage_loop() ->
    receive
        {From, _, _, _} = Request ->
            storage_handle_op(Request),
            From ! ok,
            ?MODULE:storage_loop()
    end.

storage_handle_op({_From, put, _Key, _Value}) ->
    %%timer:sleep(30),
    ok;
storage_handle_op({_From, get, _Key, _Value}) ->
    %%timer:sleep(10),
    ok.



cleanup() ->
    [exit(whereis(Name), kill) || Name <- server_names()].


server_names() ->
    lists:map(fun (N) ->
                      list_to_atom("server_" ++ integer_to_list(N))
              end, lists:seq(1, 50)).

server_for(Key) ->
    Servers = server_names(),
    I = erlang:phash(Key, length(Servers)),
    lists:nth(I, Servers).

run_foo() ->
    kprof:do_apply(?MODULE, foo, []).


foo() -> bar().
bar() -> bar.
