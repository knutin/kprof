%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Handle trace messages
%%
%% start/1 is spawned by kprof_server when the user starts a
%% trace. All trace messages are forwarded to this process, which is
%% responsible for actually finding the request.
-module(kprof_tracer).

-export([start/1]).

-export([loop/2, do_receive/1, take_request/1, make_calls/1, process_messages/1,
         take_return/3]).


start(Parent) ->
    loop(Parent, []).


loop(Parent, Acc) ->
    erlang:send_after(100, self(), process),
    Acc1 = lists:reverse(do_receive(Acc)),
    Start = now(),
    A = case process_messages(Acc1) of
            {[], NewAcc} ->
                NewAcc;
            {Requests, NewAcc} ->
                End = now(),
                io:format("Found ~p calls(~p messages) in ~p ms~n",
                          [length(Requests),
                           length(Acc1),
                           timer:now_diff(End, Start) / 1000.0]),

                Parent ! {trace_results, Requests},
                NewAcc
        end,


    case A of
        [] -> ok;
        _ ->
            io:format("AccLen: ~p~n", [length(A)]),
            io:format("A: ~p~n", [A]),
            ok
    end,

    ?MODULE:loop(Parent, lists:reverse(A)).

process_messages(Messages) ->
    process_messages(Messages, []).

process_messages(Messages, CallsAcc) ->
    {Request, NewMessages} = take_request(Messages),
    case make_calls(Request) of
        [] ->
            {CallsAcc, NewMessages};
        Calls ->
            process_messages(NewMessages, [Calls | CallsAcc])
    end.

take_request(L0) ->
    case find_entry_point(L0) of
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



find_entry_point(Messages) ->
    find_entry_point(Messages, []).

find_entry_point([{trace_ts, _, call, {sample_app, handle_op, Args}, _, _} = Msg | Rest], L)
  when length(Args) =:= 4 ->
    {Msg, lists:reverse(L, Rest)};
find_entry_point([Msg | Rest], L) ->
    find_entry_point(Rest, [Msg | L]);
find_entry_point([], L) ->
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
            Messages;
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



