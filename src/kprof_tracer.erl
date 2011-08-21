%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Handle trace messages
%%
%% start/1 is spawned by kprof_server when the user starts a
%% trace. All trace messages are forwarded to this process, which is
%% responsible for actually finding the request.
-module(kprof_tracer).

-export([start/2]).

-export([loop/3, do_receive/1, take_request/2, make_calls/1, process_messages/2,
         take_return/3]).

-define(PROCESS_INTERVAL, 100).


start(Parent, EntryPoint) ->
    erlang:send_after(?PROCESS_INTERVAL, self(), process),
    loop(Parent, EntryPoint, []).


loop(Parent, EntryPoint, Acc) ->
    Acc1 = do_receive(lists:reverse(Acc)),
    {Requests, NewAcc} = process_messages(EntryPoint, Acc1),
    Parent ! {trace_results, Requests},

    erlang:send_after(?PROCESS_INTERVAL, self(), process),
    ?MODULE:loop(Parent, EntryPoint, NewAcc).

process_messages(EntryPoint, Messages) ->
    process_messages(EntryPoint, Messages, []).

process_messages(EntryPoint, Messages, CallsAcc) ->
    {Request, NewMessages} = take_request(EntryPoint, Messages),
    case make_calls(Request) of
        [] ->
            {CallsAcc, NewMessages};
        Calls ->
            process_messages(EntryPoint, NewMessages, [Calls | CallsAcc])
    end.

take_request(EntryPoint, L0) ->
    case find_entry_point(EntryPoint, L0) of
        {undefined, _} ->
            {[], L0};
        {Msg, L1} ->
            {trace_ts, Pid, call, {M, F, Args}, {_, Label, _, _, _}, _} = Msg,
            A = length(Args),

            case take_request(Label, Msg, Pid, {M, F, A}, L1) of
                {[], _} ->
                    %% Entry point has not yet returned, wait for more calls
                    {[], L0};
                {Request, L2} ->
                    case lists:last(Request) of
                        {trace_ts, Pid, return_from, {M, F, A}, _, _} ->
                            {Request, L2};
                        _ ->
                            {[], L0}
                    end
            end
    end.
                

take_request(Label, InitialMsg, InitialPid, InitialMFA, Messages) ->
    take_request(Label, InitialMsg, InitialPid, InitialMFA, Messages, []).

take_request(_Label, _InitialMsg, _InitialPid, _InitialMFA, [], L) ->
    {[], lists:reverse(L)};

take_request(Label, InitialMsg, InitialPid, InitialMFA, [Msg | Messages], L) ->
    case Msg of
        {trace_ts, NewPid, call, {M, F, Args}, {_, Label, _, _, _}, _} ->
            {Children, L1} = take_request(Label, Msg, NewPid, {M, F, length(Args)}, Messages, L),
            {Return, L2} = take_request(Label, InitialMsg, InitialPid, InitialMFA, L1),

            {[InitialMsg, Msg] ++ Children ++ Return, L2};

        {trace_ts, InitialPid, return_from, InitialMFA, _,  _} = ReturnMsg ->
            {[ReturnMsg], lists:reverse(L, Messages)};
        {trace_ts, InitialPid, exception_from, InitialMFA, _,  _} = ReturnMsg ->
            {[ReturnMsg], lists:reverse(L, Messages)};
        _Other ->
            take_request(Label, InitialMsg, InitialPid, InitialMFA, Messages, [Msg | L])
    end.



find_entry_point(EntryPoint, Messages) ->
    find_entry_point(EntryPoint, Messages, []).

find_entry_point({M, F, A}, [{trace_ts, _, call, {M, F, Args}, _, _} = Msg | Rest], L)
  when length(Args) =:= A ->
    {Msg, lists:reverse(L, Rest)};
find_entry_point(EntryPoint, [Msg | Rest], L) ->
    find_entry_point(EntryPoint, Rest, [Msg | L]);
find_entry_point(_EntryPoint, [], L) ->
    {undefined, lists:reverse(L)}.


%% @doc: Takes a list of trace messages(call, return_from,
%% exception_from) and converts it into a list of calls, with their
%% argument, return values and timings.
make_calls(Messages) ->
    make_calls(Messages, [], -1).

make_calls([], Calls, _Depth) ->
    lists:reverse(Calls);

make_calls([{trace_ts, Pid, call, {M, F, Args}, _, StartTime} | Messages], Calls, Depth) ->
    case lists:keyfind({M, F, length(Args)}, 4, Messages) of
        {trace_ts, Pid, What, {M, F, A}, RetVal, EndTime}
          when What =:= return_from;
               What =:= exception_from ->
            ElapsedUs = timer:now_diff(EndTime, StartTime),
            NewDepth = Depth + 1,

            Call = {{M, F, A}, Args, RetVal, ElapsedUs, NewDepth},
            make_calls(Messages, [Call | Calls], NewDepth)
%%         false ->
%%             Call = {{M, F, length(Args)}, Args, undefined, -1, Depth},
%%             make_calls(Messages, [Call | Calls], Depth)
    end;

make_calls([{trace_ts, _, What, _, _, _} | Messages], Calls, Depth)
  when What =:= return_from;
       What =:= exception_from ->
    make_calls(Messages, Calls, Depth - 1).

%% %% Garbage collection
%% make_calls([{trace_ts, _, gc_start, StartInfo, StartTime} | Messages], State) ->
%%     case lists:keyfind(gc_end, 3, Messages) of
%%         {trace_ts, Pid, gc_end, EndInfo, EndTime} ->
%%             {heap_size, StartHeap} = lists:keyfind(heap_size, 1, StartInfo),
%%             {heap_size, EndHeap} = lists:keyfind(heap_size, 1, EndInfo),

%%             Stats = [{heap_diff, StartHeap - EndHeap}, {pid, Pid}],
%%             ElapsedUs = timer:now_diff(EndTime, StartTime),

%%             Call = {garbage_collection, [], Stats, ElapsedUs, State#state.depth},

%%             make_calls(Messages, State#state{calls = [Call | State#state.calls]});
%%         false ->
%%             %% Found no matching end of garbage collection, probably
%%             %% wasn't sent while we were still tracing this request
%%             make_calls(Messages, State)
%%     end;
%% make_calls([{trace_ts, _, gc_end, _, _} | Messages], State) ->
%%     make_calls(Messages, State).


do_receive(Messages) ->
    receive
        process ->
            lists:reverse(Messages);
        TraceMsg when element(1, TraceMsg) =:= trace_ts ->
            ?MODULE:do_receive([TraceMsg | Messages])
    end.



take_return(Messages, Pid, MFA) ->
    take_return(Messages, [], Pid, MFA).

take_return([], Acc, _Pid, _MFA) ->
    {undefined, lists:reverse(Acc)};

take_return([{trace_ts, Pid, What, {M, F, A}, _, _} = Msg| Rest],
            Acc, Pid, {M, F, A}) when What =:= return_from;
                                      What =:= exception_from ->
    {Msg, lists:reverse(Acc, Rest)};

take_return([Msg | Rest], Acc, Pid, MFA) ->
    take_return(Rest, [Msg | Acc], Pid, MFA).



