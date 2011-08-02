%% @author Knut Nesheim <knutin@gmail.com>
%% @copyright 2011 Knut Nesheim
%%
%% @doc Handle trace messages for a request
%%
%% start/2 is spawned by kprof_server the first time a new token label
%% is encountered. Any subsequent calls in this sequence is sent to
%% this proces. When the request finishes (the initial call returns),
%% parent is notified. The trace messages are then processed and the
%% results also sent to the parent.
-module(kprof_tracer).

-export([start/2]).

-record(state, {label,
                calls = [],
                depth = -1}).

start(Parent, Label) ->
    {TraceMessages, Pids} = do_receive(),
    Parent ! {trace_completed, Label, Pids},

    Calls = make_calls(lists:reverse(TraceMessages), #state{label = Label}),
    Parent ! {trace_results, Label, Calls}.


%% @doc: Takes a list of trace messages and converts it into a list of
%% calls, with their argument, return values and timings.
make_calls([], State) ->
    lists:reverse(State#state.calls);

make_calls([{trace_ts, _MyPid, call, {M, F, Args}, {_, _Label, _, _, _}, StartTime}
            | Messages], State) ->
    case lists:keyfind({M, F, length(Args)}, 4, Messages) of
        {trace_ts, _MaybeMyPid, What, {M, F, A}, RetVal, EndTime}
          when What =:= return_from;
               What =:= exception_from ->
            ElapsedUs = timer:now_diff(EndTime, StartTime),
            Depth = State#state.depth + 1,

            Call = {{M, F, A}, Args, RetVal, ElapsedUs, Depth},
            NewState = State#state{calls = [Call | State#state.calls],
                                   depth = Depth},
            make_calls(Messages, NewState);
        false ->
            Call = {{M, F, length(Args)}, Args, undefined, -1, State#state.depth},
            make_calls(Messages, State#state{calls = [Call | State#state.calls]})
    end;

make_calls([{trace_ts, _, What, _, _, _} | Messages], State)
  when What =:= return_from;
       What =:= exception_from ->
    make_calls(Messages, State#state{depth = State#state.depth - 1});

%% Garbage collection
make_calls([{trace_ts, _, gc_start, StartInfo, StartTime} | Messages], State) ->
    case lists:keyfind(gc_end, 3, Messages) of
        {trace_ts, Pid, gc_end, EndInfo, EndTime} ->
            {heap_size, StartHeap} = lists:keyfind(heap_size, 1, StartInfo),
            {heap_size, EndHeap} = lists:keyfind(heap_size, 1, EndInfo),

            Stats = [{heap_diff, StartHeap - EndHeap}, {pid, Pid}],
            ElapsedUs = timer:now_diff(EndTime, StartTime),

            Call = {garbage_collection, [], Stats, ElapsedUs, State#state.depth},

            make_calls(Messages, State#state{calls = [Call | State#state.calls]});
        false ->
            %% Found no matching end of garbage collection, probably
            %% wasn't sent while we were still tracing this request
            make_calls(Messages, State)
    end;
make_calls([{trace_ts, _, gc_end, _, _} | Messages], State) ->
    make_calls(Messages, State).



%% @doc: Receives all trace messages coming in as fast as possible,
%% keeps track of pids seen. Returns when the first call returns.
do_receive() ->
    do_receive([], ordsets:new()).

do_receive(Messages, Pids) ->
    receive
        {trace_ts, Pid, call, {M, F, Args}, _, _} = Msg ->
            do_receive([Msg | Messages],
                       ordsets:add_element(Pid, Pids),
                       {M, F, length(Args)})
    end.

do_receive(Messages, Pids, InitialMFA) ->
    receive
        TraceMsg when element(1, TraceMsg) =:= trace_ts ->
            case TraceMsg of
                {trace_ts, Pid, call, _, _, _} = Msg ->
                    do_receive([Msg | Messages],
                               ordsets:add_element(Pid, Pids),
                               InitialMFA);
                {trace_ts, _Pid, gc_start, _, _} = Msg ->
                    do_receive([Msg | Messages],
                               Pids,
                               InitialMFA);
                {trace_ts, _Pid, gc_end, _, _} = Msg ->
                    do_receive([Msg | Messages],
                               Pids,
                               InitialMFA);
                {trace_ts, _, What, MFA, _, _} = Msg
                  when What =:= return_from;
                       What =:= exception_from ->
                    case InitialMFA of
                        MFA ->
                            {[Msg | Messages], Pids};
                        _ ->
                            do_receive([Msg | Messages], Pids, InitialMFA)
                    end
            end
    end.
