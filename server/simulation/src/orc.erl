-module(orc).
-export([birth/2]).


birth(Name, World) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  X = random:uniform(50),
  Y = random:uniform(50),
  State = [{hit_points, 10}, {strength, 10}, {food, 3}],
  Me = [self(), Name, X, Y, State],
  World ! {Me, {birth, "It's a boy!"}},
  live(World, Me).

live(World, Me = [Self, Name, X, Y, State]) ->
  receive

    {Him, {birth, _}} ->
      [Target, HisName, HisX, HisY, HisState] = Him, 
      DistX = HisX - X,
      DistY = HisY - Y,
      Dist = math:sqrt((DistX * DistX) + (DistY * DistY)),
      case Dist < 5 of
        true ->
          react_time(random:uniform(5000)),
          Target ! {Me, {threaten, "You're dead!!"}},
          say(Name, HisName, "You're dead!!");
        false -> cant_see
      end;

    {Him, {threaten, Msg}} ->
      [Target, HisName, HisX, HisY, HisState] = Him, 
      react_time(random:uniform(5000)),
      Target ! {Me, {threaten_back, "No you're dead!!~n"}},
      say(Name, HisName, "No You're dead!!!")

  end,
  live(World, Me).

say(From, To, Msg) ->
  io:format("~s -> ~s: ~s~n",[From, To, Msg]).

react_time(Time) ->
  receive
    after Time ->
        true
    end.
