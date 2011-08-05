%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc App for generating trace events for development of kprof
-module(sample_app).
-compile([export_all]).

%% @doc: Starts tracing, dump to couchdb on localhost
t() ->
    kprof:stop(),
    kprof:start(),

    TierConfig = [{http, {?MODULE, http, 1}},
                  {database, {?MODULE, database, 0}},
                  {rendering, {?MODULE, render_response, 0}},
                  {logging, {?MODULE, logging, 0}}],

    IdentityF = fun ({?MODULE, http, [Seed]}) ->
                        case Seed rem 5 of
                            0 -> fast_call;
                            _ -> other_call
                        end;
                    (_)      -> undefined
                end,

    CouchdbOptions = [{hostname, "localhost"},
                      {port, 5984},
                      {dbname, "kprof_couchapp"},
                      {username, []},
                      {password, []}],

    kprof:start_trace([{tier_config, TierConfig},
                       {identity_f, IdentityF},
                       {print_calls, false},
                       {stats_dumper, {couchdb, CouchdbOptions}}]).

%% @doc: Run N requests
r() ->
    spawn(fun() ->
                  start_processes(),
                  r(100000),
                  cleanup()
          end).

r(0) ->
    done;
r(N) ->
    run(N),
    timer:sleep(100),
    r(N-1).

run(Seed) ->
    spawn(fun() ->
                  kprof:do_apply(?MODULE, webserver_handler, [Seed])
          end).

webserver_handler(Seed) ->
    random:seed(Seed * Seed,
                Seed * (Seed * 3),
                Seed * Seed * Seed * Seed),
    http(Seed).


http(_Req) ->
    database(),
    logging(),
    render_response().

database() ->
    random_server() ! {self(), request, 30},
    receive
        response ->
            ok
    end.

logging() ->
    random_server() ! {self(), request, 1},
    receive
        response ->
            ok
    end.

render_response() ->
    random_server() ! {self(), request, 10},
    receive
        response ->
            ok
    end.


random() ->
    round(random:uniform() * 100).



start_processes() ->
    lists:map(fun (Name) ->
                      case whereis(Name) of
                          undefined ->
                              spawn(fun() ->
                                            register(Name, self()),
                                            server_loop()
                                    end);
                          _ ->
                              []
                      end
              end, server_names()).


server_loop() ->
    receive
        {From, request, N} ->
            timer:sleep(N + random()),
            From ! response,
            ?MODULE:server_loop()
    end.


cleanup() ->
    [exit(whereis(Name), kill) || Name <- server_names()].


server_names() ->
    lists:map(fun (N) ->
                      list_to_atom("server_" ++ integer_to_list(N))
              end, lists:seq(1, 2)).

random_server() ->
    R = trunc(random:uniform() * length(server_names())) + 1,
    lists:nth(R, server_names()).
