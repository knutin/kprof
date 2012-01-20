%% @doc Server to run in target node, has helpers for setting up
%% traces, etc.
-module(kprof_target).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([enable_trace_patterns/2, disable_trace_patterns/2,
         start_trace/1, stop_trace/1,
         enable_token_server/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enable_trace_patterns(Pid, MFA) ->
    gen_server:call(Pid, {enable_trace_patterns, MFA}).

disable_trace_patterns(Pid, MFA) ->
    gen_server:call(Pid, {setup_trace_patterns, MFA}).

start_trace(Pid) ->
    gen_server:call(Pid, start_trace).

stop_trace(Pid) ->
    gen_server:call(Pid, stop_trace).

enable_token_server(Pid) ->
    gen_server:call(Pid, enable_token_server).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, []}.


handle_call(start_trace, _From, State) ->
    Port = dbg:trace_port(ip, 4711),
    %% erlang:trace(all, true, [call, set_on_spawn, timestamp,
    %%                          {tracer, Port()}]),
    erlang:trace(all, true, [call, timestamp,
                             {tracer, Port()}]),
    {reply, ok, State};

handle_call({enable_trace_patterns, MFAs}, _From, State) ->
    lists:foreach(fun (MFA) ->
                          erlang:trace_pattern(MFA,
                                               [{'_',
                                                 [{is_seq_trace}],
                                                 [{message, {get_seq_token}},
                                                  {return_trace}]}],
                                               [local])
                  end, MFAs),
    %% error_logger:info_msg("MFA: ~p~n", [MFAs]),
    %% erlang:trace_pattern({sample_app, '_', '_'},
    %%                      [{'_',
    %%                        [{is_seq_trace}],
    %%                        []
    %%                        }],
    %%                      [local]),
    {reply, ok, State};

handle_call({disable_trace_patterns, MFAs}, _From, State) ->
    lists:foreach(fun (MFA) ->
                          erlang:trace_pattern(MFA, false, [local])
                  end, MFAs),
    {reply, ok, State};
handle_call(enable_token_server, _From, State) ->
    {ok, _TokenPid} = kprof_token_server:start_link(),
    kprof_token_server:enable(),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    error_logger:info_msg("~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
