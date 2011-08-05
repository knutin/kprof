%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Public API
-module(kprof).

-export([start/0, stop/0]).

-export([start_trace/1, stop_trace/0, set_token/0, clear_token/0]).

-export([do_apply/3,
         misultin_req_loop/2, handle_misultin_req/3]).

%%
%% API
%%

start() -> application:start(kprof).
stop() -> application:stop(kprof).

%% @doc Start tracing with the given tier configuration. IdentityFun
%% must take a MFA tuple and return the identity of this sequence.
start_trace(Options) ->
    kprof_server:start_trace(Options).

stop_trace() ->
    kprof_server:stop_trace().

%% @doc: Wrapper around erlang:apply/3. Any code executed is traced.
do_apply(M, F, A) ->
    ok = set_token(),
    Result = erlang:apply(M, F, A),
    ok = clear_token(),
    Result.

%% @doc: Returns a fun suitable for use with misultin. Any code
%% executed is traced.
misultin_req_loop(M, F) ->
    fun (R) ->
            ?MODULE:handle_misultin_req(M, F, R)
    end.

handle_misultin_req(M, F, R) ->
    ok = set_token(),
    Res = M:F(R),
    ok = clear_token(),
    Res.


%%
%% INTERNAL HELPERS
%%

clear_token() ->
    seq_trace:set_token([]),
    ok.

%% @doc: Tries to get a trace token, if the token server is
%% running. Does a gen_server call with timeout of 1 millisecond to
%% avoid tracing at all if the system is loaded.
set_token() ->
    case whereis(kprof_token_server) of
        undefined ->
            ok;
        _Pid ->
            case catch(kprof_token_server:get_token(1)) of
                {ok, Label} when is_integer(Label) ->
                    seq_trace:set_token(label, Label),
                    ok;
                _ ->
                    ok
            end
    end.
