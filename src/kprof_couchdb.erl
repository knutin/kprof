-module(kprof_couchdb).

-export([put_tier_timings/1]).

-define(HEADERS, [{"Content-Type", "application/json"}]).

put_tier_timings(Timings) ->
    Body = jiffy:encode({[{docs, Timings}]}),
    do_post(url(), Body).


do_post(Url, Body) ->
    Options = [{response_format, binary}],
    case ibrowse:send_req(Url, ?HEADERS, post, Body, Options) of
        {ok, _Status, _ResponseHeaders, ResponseBody} ->
            Response = jiffy:decode(ResponseBody),
            error_logger:info_msg("Response: ~p~n", [Response]),
            ok;
        {error, Reason} ->
            error_logger:info_msg("error: ~p~n", [Reason]),
            ok
    end.    


url() ->
    lists:flatten(["http://localhost:5984", "/kprof_couchapp/_bulk_docs"]).
