-module(sample_app).

-compile([export_all]).

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

    kprof:start_trace([{tier_config, TierConfig},
                       {identity_f, IdentityF},
                       {print_calls, false},
                       {stats_dumper, {couchdb, []}}]).
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
    timer:sleep(50 + random()),
    ok.

logging() ->
    timer:sleep(1 + random()),
    ok.

render_response() ->
    timer:sleep(10 + random()).

random() ->
    round(random:uniform() * 100).
