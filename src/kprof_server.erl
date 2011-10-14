%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Setup/teardown traces, handles trace messages
%%
%% At present, there can only be one trace running at a time. There
%% may however be multiple requests executing at the same time.
%%
%% Every sequence is handled by a kprof_tracer process. When a
%% function is called and includes a trace label, that trace message
%% is forwarded to the tracer. All function calls returning in that
%% process is directed to the handler for the duration of the
%% request. This assumes the process must finish executing the
%% function before executing another function, which is not true when
%% handling system messages, but it should be true for normal cases,
%% even in a system under load.
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
    case dict:find(tier_config, State) of
        {ok, _} ->
            {reply, {error, already_tracing}, State};
        error ->
            TierConfig        = get_required_option(tier_config, Options),
            IdentityF         = get_option(identity_f, Options, fun (_) -> undefined end),
            SlowCallThreshold = get_option(slow_call_threshold, Options, -1),
            SlowCallCallback  = get_option(slow_call_callback, Options, fun (_) -> ok end),
            StatsDumper       = get_option(stats_dumper, Options, false),

            Parent = self(),
            {_, EntryPoint} = hd(TierConfig),
            TracerPid = spawn_link(fun() ->
                                           register(kprof_tracer, self()),
                                           kprof_tracer:start(Parent, EntryPoint)
                                   end),

            NewState = store([{tier_config, TierConfig},
                              {identity_f, IdentityF},
                              {slow_call_threshold, SlowCallThreshold},
                              {slow_call_callback, SlowCallCallback},
                              {tracer, TracerPid}], State),

            ok = kprof_aggregator:clear(),
            ok = kprof_aggregator:set_dumper(StatsDumper),
            ok = setup_trace_patterns(TierConfig),
            ok = do_start_trace(),
            ok = kprof_token_server:enable(),

            {reply, ok, NewState}
    end;

%% STOP TRACE
handle_call(stop_trace, _From, State) ->
    {ok, TierConfig} = dict:find(tier_config, State),
    ok = disable_trace_patterns(TierConfig),
    ok = do_stop_trace(),
    ok = kprof_token_server:disable(),

    State1 = dict:erase(tier_config, State),
    State2 = dict:erase(identity_f, State1),

    {reply, ok, State2}.


handle_cast(_Msg, State) ->
    {noreply, State}.

%% HANDLE TRACE MESSAGES
handle_info(TraceMsg, State) when element(1, TraceMsg) =:= trace_ts ->
    TracerPid = dict:fetch(tracer, State),
    TracerPid ! TraceMsg,
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


setup_trace_patterns([]) ->
    ok;
setup_trace_patterns([{_Tier, MFA} | Rest]) ->
    erlang:trace_pattern(MFA,
                         [{'_',
                           [{is_seq_trace}],
                           [{message, {get_seq_token}},
                            {return_trace}]}],
                         [local]),
    setup_trace_patterns(Rest).

disable_trace_patterns([]) ->
    ok;
disable_trace_patterns([{_Tier, MFA} | Rest]) ->
    erlang:trace_pattern(MFA, false, [local]),
    disable_trace_patterns(Rest).


do_start_trace() ->
    erlang:trace(all, true, [call, set_on_spawn, timestamp]),
    ok.

do_stop_trace() ->
    erlang:trace(all, false, [call, timestamp]),
    ok.

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
