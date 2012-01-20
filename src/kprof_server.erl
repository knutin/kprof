%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Manage trace configuration
%%
-module(kprof_server).
-behaviour(gen_server).

%% API
-export([start_link/0, start_trace/1, stop_trace/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_trace(Options) ->
    gen_server:call(?MODULE, {start_trace, Options}).

stop_trace() ->
    gen_server:call(?MODULE, stop_trace).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    seq_trace:reset_trace(),
    {ok, dict:new()}.

%% START TRACE
handle_call({start_trace, Options}, _From, State) ->
    Node              = get_required_option(node, Options),
    TierConfig        = get_required_option(tier_config, Options),
    IdentityF         = get_option(identity_f, Options,
                                   fun (_) -> undefined end),
    SlowCallThreshold = get_option(slow_call_threshold, Options, -1),
    SlowCallCallback  = get_option(slow_call_callback, Options,
                                           fun (_) -> ok end),
    StatsDumper       = get_option(stats_dumper, Options, false),
    RequestTarget     = get_option(request_target, Options, self()),

    NewState = store([{node, Node},
                      {tier_config, TierConfig},
                      {identity_f, IdentityF},
                      {slow_call_threshold, SlowCallThreshold},
                      {slow_call_callback, SlowCallCallback},
                      {request_target, RequestTarget}], State),

    {_, EntryPoint} = hd(TierConfig),

    {ok, Pid} = start_target(Node),

    ok = kprof_target:enable_trace_patterns(Pid, tc2mfa(TierConfig)),
    ok = kprof_target:start_trace(Pid),
    ok = kprof_target:enable_token_server(Pid),
    ok = start_tracer(EntryPoint, RequestTarget),

    ok = kprof_aggregator:clear(),
    ok = kprof_aggregator:set_dumper(StatsDumper),

    %%ok = kprof_token_server:enable(),

    {reply, ok, NewState};



%% STOP TRACE
handle_call(stop_trace, _From, State) ->
    %%{ok, TierConfig} = dict:find(tier_config, State),
    %% ok = disable_trace_patterns(TierConfig),
    %% ok = do_stop_trace(),
    ok = kprof_token_server:disable(),

    State1 = dict:erase(tier_config, State),
    State2 = dict:erase(identity_f, State1),

    {reply, ok, State2}.


handle_cast(_Msg, State) ->
    {noreply, State}.

%% HANDLE TRACE MESSAGES
handle_info(TraceMsg, State) when element(1, TraceMsg) =:= trace_ts ->
    error_logger:info_msg("got trace msg: ~p~n", [TraceMsg]),
    %%TracerPid = dict:fetch(tracer, State),
    %%TracerPid ! TraceMsg,
    {noreply, State};

handle_info({trace_results, Requests}, State) ->
    RequestF =
        fun (Calls) ->
                Identity = identity(Calls, get_conf(identity_f, State)),
                Threshold = get_conf(slow_call_threshold, State),
                case hd(Calls) of
                    {_, _, _, ElapsedUs, _} when ElapsedUs >= Threshold ->
                        CallbackF = get_conf(slow_call_callback, State),
                        CallbackF(Calls, Identity);
                    _ ->
                        ok
                end,

                {ok, TierConfig} = dict:find(tier_config, State),
                Timings = lists:map(fun ({MFA, _, _, ElapsedUs, _}) ->
                                            {mfa2tier(MFA, TierConfig), ElapsedUs}
                                    end, Calls),

                kprof_aggregator:tier_timings(Timings, Identity)
        end,
    lists:foreach(RequestF, Requests),

    {noreply, State};

handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    NewState = case dict:find({gc_traced, Pid}, State) of
                   {ok, true} ->
                       dict:erase({gc_traced, Pid}, State);
                   error ->
                       State
               end,
    {noreply, NewState};

handle_info(_Info, State) ->
    error_logger:info_msg("kprof_server received ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


start_target(Node) ->
    rpc:call(Node, kprof_target, start_link, []).


start_tracer(EntryPoint, RequestTarget) ->
    {F, TracerState} = kprof_tracer:mk_tracer(EntryPoint, RequestTarget),
    Pid = dbg:trace_client(ip, {"127.0.0.1", 4711}, {F, TracerState}),
    erlang:monitor(process, Pid),
    ok.


mfa2tier(garbage_collection, _TC) ->
    garbage_collection;
mfa2tier({M, F, A}, TC) when is_list(A) ->
    mfa2tier({M, F, length(A)}, TC);
mfa2tier({M, F, A}, TC) when is_integer(A) ->
    case lists:keyfind({M, F, A}, 2, TC) of
        {Tier, _} ->
            Tier;
        false ->
            undefined
    end.


identity([{garbage_collection, _, _, _, _} | Rest], IdentityF) ->
    identity(Rest, IdentityF);
identity([{{M, F, _}, Args, _, _, _} | Rest], IdentityF) ->
    case IdentityF({M, F, Args}) of
        undefined ->
            identity(Rest, IdentityF);
        Id ->
            Id
    end;
identity([], _IdentityF) ->
    undefined.

%% @doc: Starts tracing garbage collection for the given pid. The pid
%% is monitored and if it dies, it is removed from our state.
%% maybe_trace_gc(Pid, State) ->
%%     case dict:find({gc_traced, Pid}, State) of
%%         {ok, true} ->
%%             State;
%%         error ->
%%             catch(erlang:trace(Pid, true,
%%                                [call, timestamp, garbage_collection])),
%%             erlang:monitor(process, Pid),
%%             dict:store({gc_traced, Pid}, true, State)
%%     end.

get_required_option(Key, Options) ->
    case lists:keyfind(Key, 1, Options) of
        false ->
            throw({kprof, missing_required_option, Key});
        {Key, Value} ->
            Value
    end.

get_option(Key, Options, Default) ->
    case lists:keyfind(Key, 1, Options) of
        false ->
            Default;
        {Key, Value} ->
            Value
    end.

get_conf(Key, State) ->
    case dict:find(Key, State) of
        {ok, Value} ->
            Value;
        error ->
            throw({kprof, configuration_key_not_found, Key})
    end.


store([], State) ->
    State;
store([{Key, Value} | Rest], State) ->
    store(Rest, dict:store(Key, Value, State)).

tc2mfa(TierConfig) ->
    lists:map(fun ({_, MFA}) -> MFA end, TierConfig).
