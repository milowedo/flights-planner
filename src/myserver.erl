-module(myserver).
-author("Millosz").
-compile(export_all).
-compile(nowarn_export_all).

-record(state, {planes}). %% PLANES AND CLIENTS LISTS
-record(plane, {planeId="", pid, timeout={{1997,5,24},{0,0,0}}, flytime = {0,0,10}}).

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).
  %io:format("Hi, module:~p spawned:~p~nPlane name   leave date  leave hour     flight time~nAdding plane example: myserver:addPlane(\"Boeing 2779\", {{y,m,d}, {hr,min,sec}}, {2, 20, 30})~n", [?MODULE, Pid]).

init() ->
  mainServer(#state{planes =orddict:new()}).

kill() -> ?MODULE ! shutdown.

mainServer(S=#state{}) ->
  %io:format("1.ADD PLANE~n2.CANCEL PLANE~n3.LIST ALL PLANES~n4.KILL SERVER~n"),
  receive
    {info, Msg} -> %% MESSAGES
      io:format("~nINFO: ~p~n", [Msg]), mainServer(S);

    {DelegatedFrom, {add, PlaneID, Time, Duration}} -> %% ADDING
      case valid_datetime(Time) of
        true ->
          NewPlanePid = plane:start(PlaneID, Time, Duration, self()),
          NewPlanes =
            orddict:store(PlaneID,
              #plane{planeId=PlaneID, pid= NewPlanePid, timeout=Time, flytime=Duration},
              S#state.planes),
          DelegatedFrom ! {ok, NewPlanePid},
          mainServer(S#state{planes = NewPlanes});
      false ->
        DelegatedFrom ! {bad, "Check the time argument"},
        mainServer(S)
    end;


    {DelegatedFrom, {cancel, PlaneID}} -> %% CANCELLING
      case orddict:find(PlaneID, S#state.planes) of
                        {ok, E} ->
                          plane:cancel(E#plane.pid),
                          PlanesChanged =
                            orddict:erase(PlaneID, S#state.planes),
                          io:format("Plane: ~p has been cancelled", [PlaneID]),
                          DelegatedFrom ! {ok},
                          mainServer(S#state{planes=PlanesChanged});
                        error ->
                          io:format("No such plane"),
                          DelegatedFrom ! {bad, "No such plane"},
                          mainServer(S)
                      end;

    {done, PlaneID} -> %% PROCESS DONE
      case orddict:find(PlaneID, S#state.planes) of
                        {ok, E} ->
                          plane:cancel(E#plane.pid),
                          PlanesChanged =
                            orddict:erase(PlaneID, S#state.planes),
                          mainServer(S#state{planes=PlanesChanged});
                        error -> mainServer(S)
                      end;

    "PLANES" -> %% PRINTING
      case orddict:is_empty(S#state.planes) of
        false ->
          myserver:printPlanes("", S#state.planes),
          ok,
          mainServer(S);
        true ->
          io:format("No planes settled~n"),
          c:flush(),
          mainServer(S)
      end;


    shutdown -> exit(shutdown) %% SHUTTING DOWN
  end.

addPlane(PlaneName, DateStamp, TimeStamp, Duration) ->
  case whereis(?MODULE) of
    undefined ->
      io:format("SERVER IS DOWN, myserver:startServer(). TO LAUNCH SERVER~n");
    _ ->
      ?MODULE ! {self(), {add, PlaneName, {DateStamp, TimeStamp}, Duration}},
      receive
        {bad, Msg} -> Msg;
        {ok, Pid} -> {planeCreated, Pid}
      end
  end.

cancel(PlaneId) ->
  case whereis(?MODULE) of
    undefined ->
      io:format("SERVER IS DOWN, myserver:startServer(). TO LAUNCH SERVER~n");
    _ ->
      ?MODULE ! {self(), {cancel, PlaneId}},
      receive
        {bad, Msg} -> Msg;
        {ok} -> {planeCancelled}
      end
  end.

printDepartingTable() ->
  case whereis(?MODULE) of
    undefined ->
      io:format("SERVER IS DOWN, myserver:startServer(). TO LAUNCH SERVER~n");
    _ ->
      ?MODULE ! "PLANES"
  end.


%%
%% HELPER FUNCTIONS
%%
printPlanes(_,[]) -> true;
printPlanes(Text,[A]) -> printPlaneLastLine(Text, A);
printPlanes(Text,[A|B]) -> printPlaneNewLine(Text, A, B).

printPlaneNewLine(Text, {_,{_ ,ID, _, {{Y,M,D},{H,Mi,S}}, {Dh, Dm, Ds}}}, B ) ->
  Liney = lists:flatten(io_lib:format("~p ~p:~p:~p ~p-~p-~p duration: ~p:~p:~p~n", [ID,H,Mi,S,D,M,Y,Dh, Dm, Ds])),
  printPlanes(Text++Liney, B).

printPlaneLastLine(Text,{_,{_, ID, _, {{Y,M,D},{H,Mi,S}}, {Dh, Dm, Ds}}} ) ->
  Liney = lists:flatten(io_lib:format("~p ~p:~p:~p ~p-~p-~p duration: ~p:~p:~p~n", [ID,H,Mi,S,D,M,Y,Dh, Dm, Ds])),
  io:format(Text++Liney).

valid_datetime({Date,Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause ->false
  end;
valid_datetime(_) -> true.

valid_time({H,M,S}) -> valid_time(H,M,S).

valid_time(H,M,S) when H >= 0, H < 24,
  M >= 0, M < 60,
  S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.