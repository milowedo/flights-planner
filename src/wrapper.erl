%%%-------------------------------------------------------------------
%%% @author Millosz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. sty 2019 13:31
%%%-------------------------------------------------------------------
-module(wrapper).
-author("Millosz").
-compile(export_all).
-compile(nowarn_export_all).

start() ->
  myserver:start(),
  loop().
add({Name, Date, Time, Length}) -> myserver:addPlane(Name, Date, Time, Length).
cancel(Name) -> myserver:cancel(Name).
kill() -> myserver:kill().
getPlanes() -> myserver:printDepartingTable().

loop() ->
  printmenu().
%%  receive
%%    choice -> {yeap}
%%  end.

printmenu() ->
  io:format("~n1.ADD PLANE~n2.CANCEL PLANE~n3.LIST ALL PLANES~n4.KILL SERVER~n5.CURRENT TIME~n"),
  Line = io:get_line("ENTER CHOICE: "),
  Choice = string:strip(string:strip(Line, both, 13), both, 10),
  case Choice of
    "1" ->
      clearConsole(),
      Plane = getData(),
      clearConsole(),
      add(Plane),
      timer:sleep(600),
      printmenu();
    "2" ->
      clearConsole(),
      Name = getName(),
      cancel(Name),
      printmenu();
    "3" ->
      clearConsole(),
      getPlanes(),
      printmenu();
    "4" ->
      clearConsole(),
      kill();
    "5" ->
      clearConsole(),
      {H, M, S} = time(),
      io:format("Current time is ~p:~p:~p", [H, M, S]),
      printmenu();
    _ ->clearConsole(),
      io:format("You have entered a bad argument~n"),
      c:flush(),
      printmenu()
  end.

clearConsole() ->
  while(13).
while(0) -> io:format("~n");
while(Num) ->
  io:format("~n"),
  while(Num-1).


getName() ->
  Name = io:get_line("ENTER NAME TO CANCEL: "),
  NameVal = string:strip(string:strip(Name, both, 13), both, 10),
  NameVal.

getData() ->
  Name = io:get_line("ENTER NAME: "),
  NameVal = string:strip(string:strip(Name, both, 13), both, 10),
  getDepartureDate(NameVal).

getDepartureDate(Name)->
  DepartureDate = io:get_line("ENTER DATE OF DEPARTUE, ex.2019-01-15:  "),
  DepDVal = string:strip(string:strip(DepartureDate, both, 13), both, 10),
  Len = string:length(DepDVal),
  if
    Len == 10 ->
      getDepartureTime(Name, DepDVal);
    true ->
      io:format("You have entered the date wrongly, think again~n"),
      c:flush(),
      getDepartureDate(Name)

  end.

getDepartureTime(Name, Date)->
  DepartureTime = io:get_line("ENTER TIME OF DEPARTUE, ex.22-23-24:  "),
  DepTVal = string:strip(string:strip(DepartureTime, both, 13), both, 10),
  Len = string:length(DepTVal),
  if
    Len == 8 ->
      getLength(Name, Date, DepTVal);
    true ->
      io:format("You have entered the time wrongly, think again~n"),
      c:flush(),
      getDepartureTime(Name, Date)
  end.

getLength(Name, Date, Time)->
  Length = io:get_line("ENTER THE LENGTH OF FLIGHT, ex(4sec flight).00-00-04:  "),
  DepLVal = string:strip(string:strip(Length, both, 13), both, 10),
  Len = string:length(DepLVal),
  if
    Len == 8 ->
      {Name,
        dateToTimestamp(Date),
        timeToTimestamp(Time),
        timeToTimestamp(DepLVal)};
    true ->
      io:format("You have entered the time wrongly, think again~n"),
      c:flush(),
      getLength(Name, Date, Time)
  end.


dateToTimestamp(Line) ->
  {Year, _} = string:to_integer(string:substr(Line, 1, 4)),
  {Month, _} = string:to_integer(string:substr(Line, 6, 9)),
  {Day, _} = string:to_integer(string:substr(Line, 9, length(Line))),
  {Year,Month, Day}.

timeToTimestamp(Line) ->
  {Hr, _} = string:to_integer(string:substr(Line, 1, 2)),
  {Min, _} = string:to_integer(string:substr(Line, 4, 7)),
  {Sec, _} = string:to_integer(string:substr(Line, 7, length(Line))),
  {Hr, Min, Sec}.




