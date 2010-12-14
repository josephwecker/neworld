-module(server).
-export([start/0]).

start() ->
  Self = self(),
  Names = ["Helga", "Toth", "Rorg", "Morteg", "Bgh", "Uuuung", "Elel", "Dorkr"],
  Orcs = lists:map(
    fun(Name) ->
        spawn(
          fun() ->
              orc:birth(Name, Self)
          end)
    end, Names),
  %spawn(fun() -> watch_it(Orcs) end).
  watch_it(Orcs).

watch_it(Orcs) ->
  receive
    quit ->
      io:format("Quitting...", []),
      shucks;
    {Me, Msg} ->
      [Orc|_] = Me,
      lists:foreach(
        fun(Neighbor) ->
            case Neighbor =:= Orc of
              true -> do_nothing;
              false -> Neighbor ! {Me, Msg}
            end
        end, Orcs),
      watch_it(Orcs)
  end.
