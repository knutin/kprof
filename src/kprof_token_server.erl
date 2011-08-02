-module(kprof_token_server).

-behaviour(gen_server).

%% API
-export([start_link/0, get_token/0, get_token/1, enable/0, disable/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(PERCENTAGE, 1).

-record(state, {tokens, max, running = false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_token() ->
    get_token(100).

get_token(Timeout) ->
    gen_server:call(?MODULE, get_token, Timeout).

enable() ->
    gen_server:call(?MODULE, enable).

disable() ->
    gen_server:call(?MODULE, disable).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{tokens = sets:new(), running = false, max = 5000}}.

handle_call(get_token, _From, #state{running = false} = State) ->
    {reply, {error, trace_not_running}, State};

handle_call(get_token, _From, #state{tokens = Tokens, max = Max} = State) ->
    %% TODO: Weighted decision to sample or not
    {NewTokens, Token} = get_next_token(Tokens, Max),
    {reply, {ok, Token}, State#state{tokens = NewTokens, max = Token + 1}};

handle_call(enable, _From, State) ->
    {reply, ok, State#state{running = true}};

handle_call(disable, _From, State) ->
    {reply, ok, State#state{running = false}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_next_token(IssuedTokens, 10000) ->
    get_next_token(IssuedTokens, 0);
get_next_token(IssuedTokens, Max) ->
    get_next_token(IssuedTokens, Max, Max).

get_next_token(IssuedTokens, Max, Try) ->
    case sets:is_element(Try, IssuedTokens) of
        true ->
            get_next_token(IssuedTokens, Max, Try+1);
        false ->
            {sets:add_element(Try, IssuedTokens), Try}
    end.
    
