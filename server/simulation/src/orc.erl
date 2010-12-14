-module(orc).
-export([birth/2]).
-include("orc.hrl").

birth(Name, World) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  X = random:uniform(50),
  Y = random:uniform(50),
  New_Orc = #orc{id=self(), who=Name, x=X, y=Y},
  World ! {New_Orc, {birth, "It's a boy!"}},
  World ! {New_Orc, polo}, 
  live(World, New_Orc).

live(World, Self) ->
  World ! {Self, polo},
  receive
    {Other, polo} ->
      DistX = Other#orc.x - Self#orc.x,
      DistY = Other#orc.y - Self#orc.y,
      Dist = math:sqrt((DistX * DistX) + (DistY * DistY)),
      case Dist < 5 of
        true ->
          case Self#orc.target =:= none of
            true ->
              react_time(random:uniform(5000)),
              NewSelf = Self#orc{target=Other},
              Other#orc.id ! {Self, {threaten, "You're dead!!"}},
              say(Self#orc.who, Other#orc.who, "You're dead!!");
            false -> 
              NewSelf = Self
          end;
        false ->
          NewSelf = Self
      end;

    {Other, {threaten, _}} ->
      %react_time(random:uniform(5000)),
      %Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
      %say(Self#orc.who, Other#orc.who, "No You're dead!!!"),
      NewSelf = Self
  end,
  Target = NewSelf#orc.target,
  case Target =:= none of
    true ->
      move(World, NewSelf, random:uniform(50),random:uniform(50));
    false ->
      case Target#orc.x =:= NewSelf#orc.x andalso Target#orc.y =:=
        NewSelf#orc.y of
        true ->
          done;
        false ->
          move(World, NewSelf, Target#orc.x, Target#orc.y)
      end
  end.

say(From, To, Msg) ->
  io:format("~s -> ~s: ~s~n",[From, To, Msg]).

react_time(Time) ->
  receive
    after Time ->
      true
  end.

move(World, OldSelf, Target_x, Target_y) ->
  X = new_coords(OldSelf#orc.x, Target_x),
  Y = new_coords(OldSelf#orc.y, Target_y),
  Self = OldSelf#orc{x = X, y = Y},
  react_time(1000),
  live(World, Self).

new_coords(O, T) when O == T -> O;
new_coords(O, T) when O < T -> (O + 1);
new_coords(O, T) when O > T -> (O - 1).

