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
    {Return, NewRest} = take_return(Call, Rest),
    [{mfa(Call), elapsed(Call, Return), request2graph(NewRest)}].


take_return(Call, Msgs) ->
    {value, Return, NewMsgs} = lists:keytake(mfa(Call), 4, Msgs),
    {Return, NewMsgs}.







%% process_messages(EntryPoint, Messages) ->
%%     process_messages(EntryPoint, Messages, []).

%% process_messages(EntryPoint, Messages, CallsAcc) ->
%%     try
%%         {Request, NewMessages} = take_request(EntryPoint, Messages),
%%         case make_calls(Request) of
%%             [] ->
%%                 {CallsAcc, NewMessages};
%%             Calls ->
%%                 process_messages(EntryPoint, NewMessages, [Calls | CallsAcc])
%%         end
%%     catch
%%         throw:{kprof, missing_return, _} ->
%%             {CallsAcc, Messages}
%%     end.

%% %% @doc: If a full request exists, that is the entry point is called
%% %% and returned from, all trace messages in this request is returned
%% %% together with a new list of messages where the trace messages of
%% %% the request has been removed. It is somewhat complex in order to
%% %% only traverse the (potentially very big) list of trace messages
%% %% only once for every request.
%% take_request(EntryPoint, L0) ->
%%     case take_entry_point(EntryPoint, L0) of
%%         undefined ->
%%             {[], L0};
%%         {Entry, L1} ->
%%             {trace_ts, Pid, call, {M, F, Args}, {_, Label, _, _, _}, _} = Entry,
%%             Stack = [{Label, Pid, {M, F, length(Args)}}],
%%             {Request, L2} = do_take_request(Stack, L1),
%%             {[Entry | Request], L2}
%%     end.

%% do_take_request(Stack, L) ->
%%     do_take_request(Stack, [], [], L).

%% do_take_request([], ResultAcc, MessageAcc, L) ->
%%     {lists:reverse(ResultAcc), lists:reverse(MessageAcc) ++ L};

%% do_take_request(Stack, _ResultAcc, _MessageAcc, []) when length(Stack) > 0 ->
%%     throw({kprof, missing_return, Stack});

%% do_take_request([{Label, Pid, MFA} | _] = Stack, ResultAcc, MessageAcc, [Msg | L]) ->
%%     case Msg of
%%         {trace_ts, NewPid, call, {M, F, Args}, {_, Label, _, _, _}, _} = Msg ->
%%             do_take_request([{Label, NewPid, {M, F, length(Args)}} | Stack],
%%                             [Msg | ResultAcc], MessageAcc, L);
%%         {trace_ts, Pid, return_from, MFA, _, _} ->
%%             do_take_request(tl(Stack), [Msg | ResultAcc], MessageAcc, L);
%%         _Other ->
%%             do_take_request(Stack, ResultAcc, [Msg | MessageAcc], L)
%%     end.





%% take_entry_point(EntryPoint, Messages) ->
%%     take_entry_point(EntryPoint, Messages, []).

%% take_entry_point({M, F, A}, [{trace_ts, _, call, {M, F, Args}, _, _} = Msg | Rest], L)
%%   when length(Args) =:= A ->
%%     {Msg, lists:reverse(L, Rest)};
%% take_entry_point(EntryPoint, [Msg | Rest], L) ->
%%     take_entry_point(EntryPoint, Rest, [Msg | L]);
%% take_entry_point(_EntryPoint, [], _L) ->
%%     undefined.



%% %% @doc: Takes a list of trace messages(call, return_from,
%% %% exception_from) and converts it into a list of calls, with their
%% %% argument, return values and timings.
%% make_calls(Messages) ->
%%     make_calls(Messages, [], -1).

%% make_calls([], Calls, _Depth) ->
%%     lists:reverse(Calls);

%% make_calls([{trace_ts, Pid, call, {M, F, Args}, _, StartTime} | Messages], Calls, Depth) ->
%%     case lists:keyfind({M, F, length(Args)}, 4, Messages) of
%%         {trace_ts, Pid, What, {M, F, A}, RetVal, EndTime}
%%           when What =:= return_from;
%%                What =:= exception_from ->
%%             ElapsedUs = timer:now_diff(EndTime, StartTime),
%%             NewDepth = Depth + 1,

%%             Call = {{M, F, A}, Args, RetVal, ElapsedUs, NewDepth},
%%             make_calls(Messages, [Call | Calls], NewDepth)
%% %%         false ->
%% %%             Call = {{M, F, length(Args)}, Args, undefined, -1, Depth},
%% %%             make_calls(Messages, [Call | Calls], Depth)
%%     end;

%% make_calls([{trace_ts, _, What, _, _, _} | Messages], Calls, Depth)
%%   when What =:= return_from;
%%        What =:= exception_from ->
%%     make_calls(Messages, Calls, Depth - 1).

%% %% %% Garbage collection
%% %% make_calls([{trace_ts, _, gc_start, StartInfo, StartTime} | Messages], State) ->
%% %%     case lists:keyfind(gc_end, 3, Messages) of
%% %%         {trace_ts, Pid, gc_end, EndInfo, EndTime} ->
%% %%             {heap_size, StartHeap} = lists:keyfind(heap_size, 1, StartInfo),
%% %%             {heap_size, EndHeap} = lists:keyfind(heap_size, 1, EndInfo),

%% %%             Stats = [{heap_diff, StartHeap - EndHeap}, {pid, Pid}],
%% %%             ElapsedUs = timer:now_diff(EndTime, StartTime),

%% %%             Call = {garbage_collection, [], Stats, ElapsedUs, State#state.depth},

%% %%             make_calls(Messages, State#state{calls = [Call | State#state.calls]});
%% %%         false ->
%% %%             %% Found no matching end of garbage collection, probably
%% %%             %% wasn't sent while we were still tracing this request
%% %%             make_calls(Messages, State)
%% %%     end;
%% %% make_calls([{trace_ts, _, gc_end, _, _} | Messages], State) ->
%% %%     make_calls(Messages, State).


%% do_receive(Messages) ->
%%     receive
%%         process ->
%%             lists:reverse(Messages);
%%         TraceMsg when element(1, TraceMsg) =:= trace_ts ->
%%             ?MODULE:do_receive([TraceMsg | Messages])
%%     end.



%% take_return(Messages, Pid, MFA) ->
%%     take_return(Messages, [], Pid, MFA).

%% take_return([], Acc, _Pid, _MFA) ->
%%     {undefined, lists:reverse(Acc)};

%% take_return([{trace_ts, Pid, What, {M, F, A}, _, _} = Msg| Rest],
%%             Acc, Pid, {M, F, A}) when What =:= return_from;
%%                                       What =:= exception_from ->
%%     {Msg, lists:reverse(Acc, Rest)};

%% take_return([Msg | Rest], Acc, Pid, MFA) ->
%%     take_return(Rest, [Msg | Acc], Pid, MFA).

%% %% format_msgs(Ms) ->
%% %%     S = lists:map(fun ({trace_ts, _, call, {M, F, Args}, _, _}) ->
%% %%                           io_lib:format("call    ~p~n", [{M, F, length(Args)}]);
%% %%                       ({trace_ts, _, return_from, MFA, _, _}) ->
%% %%                           io_lib:format("return  ~p~n", [MFA])
%% %%                   end, Ms),
%% %%     error_logger:info_msg(S).

