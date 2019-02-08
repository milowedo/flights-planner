-module(plane).
-compile(export_all).
-compile(nowarn_export_all).
-record( state, {server, name="", to_go=0, fly_time=0} ).

start(PlaneName, Departure, FlightTime) ->
  Printer = spawn(?MODULE, listener, []),
  spawn(?MODULE, init, [Printer, PlaneName, Departure, FlightTime]).

start(PlaneName, Departure, FlightTime, PID) ->
  spawn(?MODULE, init, [PID, PlaneName, Departure, FlightTime]).

listener() ->
  receive
    fish ->
      io:format("aww fish"), %% DEBUG FEATURE
      listener(); %% DEBUG FEATURE
    {info, Msg} ->
      io:format("~p: ~p~n", [pid_to_list(self()), Msg]),
      listener();
    _ -> ok
   after 3000 ->
    ok,
    listener()
  end.

init(Server, PlaneName, DateTime, FlightTime) ->
  Server ! {info, "Plane: " ++ PlaneName ++ " departure scheduled on " ++ format_datetime(DateTime) ++ " ::FLIGHT TIME is: " ++ format_time(FlightTime) ++ "."},
  loop(
    #state{server=Server, name= PlaneName, to_go=change_date_to_sec(DateTime), fly_time=FlightTime}
  ).

cancel(Pid) ->
%% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

loop(S = #state{server=Server, to_go=[T|Next], fly_time=FlightTime}) -> %% PLANE PROCESS RUNNING FOR REQ TIME
  receive
    {Server, Ref, cancel} -> Server ! {Ref, ok}
  after (T*1000) -> % make it miliseconds
    if (Next =:= []) ->
      Server ! {info, "Plane: " ++ S#state.name ++ " is departing, will arrive in " ++  format_time(FlightTime) ++ "."},
      waitToArrive(change_time_to_sec(FlightTime)),
      Server ! {info, "Plane: " ++ S#state.name ++ " has arrived, thank you for the save journey."},
      Server ! {done, S#state.name};
    %else
      Next =/= [] -> loop(S#state{to_go=Next}) %so it has to be a list due to 50day max timestamp val
    end
  end.

waitToArrive(Sec) ->
  receive
  after (1000 * Sec) -> ok
  end.

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

change_date_to_sec( TimeOut = { {_,_,_}, {_,_,_} } ) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0  -> ToGo;
           ToGo =< 0 -> 0
         end,
  normalize(Secs).

change_time_to_sec({H,M,S}) ->
  Hours = H*60*60,
  Minutes = M*60,
  Time = Hours+Minutes+S,
  Time.


format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).

format_time({H, M, S}) ->
  io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).