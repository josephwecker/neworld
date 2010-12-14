-module(server).
-export([start/0]).

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
    Orcs when is_list(Orcs) ->
    %[_|_] = Orcs ->
      watch_it(Orcs)
  end.

watch_it(Orcs) ->
  receive
    quit ->
      io:format("Quitting...", []),
      shucks;
    {Orc, polo} ->
     % io:format("~s: ~p,~p~n",[element(3, Orc), element(4, Orc), element(5, Orc)]),
      Id = element(2, Orc),
      lists:foreach(
        fun(Neighbor) ->
            case Neighbor =:= Id of
              true -> do_nothing;
              false -> Neighbor ! {Orc, polo}
            end
        end, Orcs),
      watch_it(Orcs)
  end.
