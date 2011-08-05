-module(kprof_couchdb).

-export([put_tier_timings/2]).

-define(HEADERS, [{"Content-Type", "application/json"}]).

put_tier_timings(Timings, Config) ->
    Body = jiffy:encode({[{docs, Timings}]}),
    do_post(url(Config), Body, Config).


do_post(Url, Body, Config) ->
    Options = case {proplists:get_value(username, Config),
                    proplists:get_value(password, Config)} of
                  {[], []} ->
                      [{response_format, binary}];
                  {Username, Password} ->
                      [{response_format, binary},
                       {basic_auth, {Username, Password}}]
              end,
    
    case ibrowse:send_req(Url, ?HEADERS, post, Body, Options) of
        {ok, _Status, _ResponseHeaders, ResponseBody} ->
            Response = jiffy:decode(ResponseBody),
            error_logger:info_msg("Response: ~p~n", [Response]),
            ok;
        {error, Reason} ->
            error_logger:info_msg("error: ~p~n", [Reason]),
            ok
    end.    


url(Config) ->
    Hostname = proplists:get_value(hostname, Config),
    Port = integer_to_list(proplists:get_value(port, Config)),
    DbName = proplists:get_value(dbname, Config),

    lists:flatten(["http://", Hostname, ":", Port, "/", DbName, "/_bulk_docs"]).
