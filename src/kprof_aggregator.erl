%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Aggregates and sends results to the kprof viewer
%%
%% For every tier and id a histogram is updated when a new timing
%% comes in. At every reporting interval, the data is shipped to the
%% kprof viewer and the histograms are reset.

-module(kprof_aggregator).

-behaviour(gen_server).

%% API
-export([start_link/0, tier_timings/2, clear/0, set_dumper/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(NEW_HISTOGRAM, basho_stats_histogram:new(0, 1000000, 1000000)).
-define(REPORT_INTERVAL, 60).

-record(state, {last_write_time, timer, dumper = false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

tier_timings(Tiers, Id) ->
    gen_server:cast(?SERVER, {tier_timings, Tiers, Id}).

clear() ->
    gen_server:call(?SERVER, clear_stats).

set_dumper(Dumper) ->
    gen_server:call(?SERVER, {set_dumper, Dumper}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ReportInterval = timer:seconds(?REPORT_INTERVAL),
    TimerRef = erlang:send_after(ReportInterval, self(), report),
    {ok, #state{timer = TimerRef}}.

handle_call(clear_stats, _From, State) ->
    ClearF = fun ({{tier_timing, _} = Key, {hist, _}}) ->
                     erlang:put(Key, {hist, ?NEW_HISTOGRAM});
                 (_) ->
                     ok
             end,
    lists:foreach(ClearF, get()),
    {reply, ok, State};

handle_call({set_dumper, Dumper}, _From, State) ->
    {reply, ok, State#state{dumper = Dumper}}.


handle_cast({tier_timing, {Tier, Id, ElapsedUs}}, State) ->
    ok = update_histogram({tier_timing, {Tier, Id}}, ElapsedUs),
    ok = update_histogram({tier_timing, {Tier, '_total'}}, ElapsedUs),
    {noreply, State};

handle_cast({tier_timings, Tiers, Id}, State) ->
    UpdateF =
        fun ({Tier, ElapsedUs}) ->
                ok = update_histogram({tier_timing, {Tier, Id}}, ElapsedUs),
                ok = update_histogram({tier_timing, {Tier, '_total'}}, ElapsedUs)
        end,
    lists:foreach(UpdateF, Tiers),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, #state{timer = OldTimerRef} = State) ->
    erlang:cancel_timer(OldTimerRef),
    Now = now_to_seconds(),
    send_stats(State#state.dumper),

    ReportInterval = timer:seconds(?REPORT_INTERVAL),
    TimerRef = erlang:send_after(ReportInterval, self(), report),
    {noreply, State#state{last_write_time = Now, timer = TimerRef}}.

terminate(_Reason, #state{timer = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_or_create_histogram(Key) ->
    case get(Key) of
        undefined     -> ?NEW_HISTOGRAM;
        {hist, Value} -> Value;
        _             ->
            throw({kingdom, unexpected_value_in_stats_server_pdict, []})
    end.

%% @doc: Updates the histogram stored under the given key in the
%% process dictionary with the given value.
update_histogram(Key, Value) ->
    Hist = get_or_create_histogram(Key),
    NewHist = basho_stats_histogram:update(Value, Hist),
    erlang:put(Key, {hist, NewHist}),
    ok.

%% @doc: Pulls statistics out from the histograms and sends it off to
%% disk or couchdb. If no handler is specified, stats are accumulated
%% in the process forever.
send_stats(false) ->
    ok;

send_stats({file, Path}) ->
    case get_tier_timings() of
        [] -> ok;
        TierTimings ->
            Data = {[{tier_timings, TierTimings}]},
            Json = jiffy:encode(Data),
            ok = file:write_file(Path, Json, [append]),
            ok
    end;

send_stats({'fun', F}) ->
    case get_tier_timings() of
        [] -> ok;
        Timings ->
            F(Timings),
            ok
    end;

send_stats({pid, Pid}) ->
    case get_tier_timings() of
        [] -> ok;
        TierTimings ->
            Data = {[{tier_timings, TierTimings}]},
            Pid ! {kprof_aggregated_data, Data},
            ok
    end;

send_stats({couchdb, CouchdbOptions}) ->
    case get_tier_timings() of
        [] -> ok;
        Timings ->
            kprof_couchdb:put_tier_timings(Timings, CouchdbOptions)
    end.


%% @doc: Extra data that will be appended to the json document
extra_data() ->
    [{node, list_to_binary(atom_to_list(node()))}].


%% @doc: Returns all tier timing histograms that actually contains an
%% observation
get_tier_timings() ->
    Ts = lists:filter(fun ({{tier_timing, _}, {hist, Hist}}) ->
                              basho_stats_histogram:observations(Hist) =/= 0;
                          (_) ->
                              false
                      end, get()),
    [erlang:put(Key, {hist, ?NEW_HISTOGRAM}) || {Key, _Value}  <- Ts],
    lists:map(fun (H) -> format_timing(H, json) end, Ts).

format_timing({{tier_timing, {Tier, Id}}, {hist, Hist}}, json) ->
    {Min, Mean, Max, _, SD} =
        basho_stats_histogram:summary_stats(Hist),

    {[{timestamp, now_to_seconds()},
      {key, list_to_binary(io_lib:format("~w.~s", [Tier, id2str(Id)]))},
      {observations, basho_stats_histogram:observations(Hist)},
      {min, Min},
      {max, Max},
      {mean, Mean},
      {sd, SD},
      {quantile_25, basho_stats_histogram:quantile(0.250, Hist)},
      {quantile_75, basho_stats_histogram:quantile(0.750, Hist)},
      {quantile_99, basho_stats_histogram:quantile(0.990, Hist)},
      {quantile_999, basho_stats_histogram:quantile(0.999, Hist)} |
      extra_data()]}.

now_to_seconds() ->
    {MegaSeconds, Seconds, _} = now(),
    MegaSeconds * 1000000 + Seconds.


id2str({Resource, Action}) when is_atom(Resource) andalso is_atom(Action) ->
    lists:flatten(io_lib:format("~w_~w", [Resource, Action]));
id2str(Key) ->
    Key.
