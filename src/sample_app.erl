-module(sample_app).

-compile([export_all]).

t() ->
    kprof:stop(),
    kprof:start(),

    TierConfig = [{http, {?MODULE, http, 1}},
                  {database, {?MODULE, database, 0}},
                  {rendering, {?MODULE, render_response, 0}},
                  {logging, {?MODULE, logging, 0}}],

    IdentityF = fun ({?MODULE, http, [foobar]}) -> foobar;
                    (_)      -> undefined
                end,

    kprof:start_trace([{tier_config, TierConfig},
                       {identity_f, IdentityF},
                       {print_calls, true},
                       {stats_dumper, {couchdb, []}}]).
                       

r(Seed) ->
    spawn(fun() ->
                  kprof:do_apply(?MODULE, webserver_handler, [Seed])
          end).
    

webserver_handler(Seed) ->
    random:seed(Seed, Seed, Seed),
    http(foobar).


http(_Req) ->
    timer:sleep(2 + random()),
    database(),
    logging(),
    render_response().

database() ->
    timer:sleep(20 + random()),
    ok.

logging() ->
    timer:sleep(1 + random()),
    ok.

render_response() ->
    timer:sleep(10 + random()).

random() ->
    round(random:uniform() * 50).

    
    
