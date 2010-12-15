-module(server).
-export([start/0]).
-include("orc.hrl").

start() ->
  Names = ["Helga", "Toth", "Rorg", "Morteg", "Bgh", "Uuuung", "Elel", "Dorkr"],
  Self = spawn(fun watch_it/0),
  Orcs = lists:map(
    fun(Name) ->
        spawn(
          fun() ->
              orc:birth(Name, Self)
          end)
    end, Names),
  Self ! Orcs,
  Self.

watch_it() ->
  receive
    [_|_] = Orcs ->
      Positions = [],
      Updater = spawn(fun() -> updater(Positions) end),
      spawn(fun() -> draw(Updater) end),
      watch_it(Orcs, Updater)
  end.

watch_it(Orcs, Updater) ->
  receive
    quit ->
      io:format("Quitting...", []),
      shucks;
    {Orc, polo} ->
      Updater ! {Orc, polo},
      io:format("(~p)~s: ~p,~p~n",[Orc#orc.hit_points,Orc#orc.who,Orc#orc.x,Orc#orc.y]),
      Id = element(2, Orc),
      lists:foreach(
        fun(Neighbor) ->
            case Neighbor =:= Id of
              true -> do_nothing;
              false -> Neighbor ! {Orc, polo}
            end
        end, Orcs),
      watch_it(Orcs, Updater)
  end.

draw(Updater) ->
  timer:send_after(1000, Updater, {update_request, self()}),
  receive 
    {pos, _} -> %Positions} ->
      %io:format("~p~n",[Positions])
      okay
  end,
  draw(Updater).

updater(Positions) ->
  receive
    {Orc, polo} ->
      New_Pos = update_positions(Positions, Orc#orc.id, Orc#orc.x, Orc#orc.y);
    {update_request, Draw} ->
      Draw ! {pos, Positions},
      New_Pos = Positions
  end,
  updater(New_Pos).

update_positions(Pos, Orc, X, Y) ->
  case lists:keymember(Orc, 1, Pos) of
    true ->
      Pos1 = lists:keyreplace(Orc, 1, Pos, {Orc,X,Y});
    false ->
      Pos1 = lists:append(Pos, [{Orc,X,Y}])
  end,
  Pos1.
