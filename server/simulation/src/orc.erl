-module(orc).
-export([birth/2]).

-record(orc, {
    id,
    who,
    x,
    y,
    hit_points = 10,
    strength = 10,
    food = 3
  }).

birth(Name, World) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  X = random:uniform(30),
  Y = random:uniform(30),
  New_Orc = #orc{id=self(), who=Name, x=X, y=Y},
  io:format("~s~n",[New_Orc#orc.who]),
  World ! {New_Orc, {birth, "It's a boy!"}},
  live(World, New_Orc).

live(World, Self) ->
  receive

    {Other, {birth, _}} ->
      DistX = Other#orc.x - Self#orc.x,
      DistY = Other#orc.y - Self#orc.y,
      Dist = math:sqrt((DistX * DistX) + (DistY * DistY)),
      case Dist < 5 of
        true ->
          react_time(random:uniform(5000)),
          Other#orc.id ! {Self, {threaten, "You're dead!!"}},
          say(Self#orc.who, Other#orc.who, "You're dead!!");
        false -> cant_see
      end;

    {Other, {threaten, Msg}} ->
      react_time(random:uniform(5000)),
      Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
      say(Self#orc.who, Other#orc.who, "No You're dead!!!")

  end,
  live(World, Self).

say(From, To, Msg) ->
  io:format("~s -> ~s: ~s~n",[From, To, Msg]).

react_time(Time) ->
  receive
    after Time ->
        true
    end.
