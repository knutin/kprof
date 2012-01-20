%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Handle trace messages
%%
%% start/1 is spawned by kprof_server when the user starts a
%% trace. All trace messages are forwarded to this process, which is
%% responsible for actually finding the request.
-module(kprof_tracer).

-export([mk_tracer/2, trace_handler/2, request2graph/1]).

-export([pid/1]).

-record(state, {traces, returns, entrypoint, request_target}).


mk_tracer(EntryPoint, RequestTarget) ->
    S = #state{traces = dict:new(), returns = dict:new(),
               request_target = RequestTarget, entrypoint = EntryPoint},
    {fun ?MODULE:trace_handler/2, S}.

trace_handler(end_of_trace, S) ->
    S;
trace_handler(Msg, S) ->
    case is_call(Msg) of
        true ->
            NewTraces = insert_msg(label(Msg), Msg, S#state.traces),
            NewReturns = insert_return(ret_key(Msg), label(Msg), S#state.returns),
            S#state{traces = NewTraces, returns = NewReturns};
        false ->
            {Label, NewReturns} = fetch_erase(ret_key(Msg), S#state.returns),
            case is_entry_return(Msg, S#state.entrypoint) of
                true ->
                    {Msgs, NewTraces} = fetch_erase(Label, S#state.traces),
                    S#state.request_target ! {trace, lists:reverse([Msg | Msgs])},
                    S#state{returns = NewReturns, traces = NewTraces};
                false ->
                    NewTraces = insert_msg(Label, Msg, S#state.traces),
                    S#state{returns = NewReturns, traces = NewTraces}
            end
    end.


insert_msg(Key, Val, D) ->
    dict:update(Key, fun (OldVals) -> [Val|OldVals] end, [Val], D).


insert_return(Key, Val, D) ->
    dict:store(Key, Val, D).


fetch_erase(Key, D) ->
    Val = dict:fetch(Key, D),
    {Val, dict:erase(Key, D)}.


ret_key(Msg) -> {pid(Msg), mfa(Msg)}.

is_entry_return(Msg, EntryPoint) -> mfa(Msg) =:= EntryPoint.

pid({trace_ts, Pid, call, _, _, _})        -> Pid;
pid({trace_ts, Pid, return_from, _, _, _}) -> Pid.

is_call({trace_ts, _, call, _, _, _})        -> true;
is_call({trace_ts, _, return_from, _, _, _}) -> false;
is_call(Msg)                                 -> throw({unexpected_msg, Msg}).

mfa({trace_ts, _, call, {M, F, Args}, _, _}) -> {M, F, length(Args)};
mfa({trace_ts, _, return_from, MFA, _, _})   -> MFA.

label({trace_ts, _, call, _, {_, Label, _, _, _}, _}) -> Label.

ts({trace_ts, _, call, _, _, Ts})        -> Ts;
ts({trace_ts, _, return_from, _, _, Ts}) -> Ts.

elapsed(Call, Return) -> timer:now_diff(ts(Return), ts(Call)).


request2graph([]) ->
    [];
request2graph(Msgs) ->
    [Call | Rest] = Msgs,

    case is_call(hd(Rest)) of
        true ->
            {Return, NewRest} = take_return(Call, Rest),
            [{mfa(Call), elapsed(Call, Return), request2graph(NewRest)}];
        false ->
            [{mfa(Call), elapsed(Call, hd(Rest)), []} | request2graph(tl(Rest))]
    end.


take_return(Call, Msgs) ->
    {value, Return, NewMsgs} = lists:keytake(mfa(Call), 4, Msgs),
    {Return, NewMsgs}.
