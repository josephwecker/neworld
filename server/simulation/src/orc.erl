-module(orc).
-export([birth/2]).
-include("orc.hrl").

birth(Name, World) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  X = random:uniform(50),
  Y = random:uniform(30),
  New_Orc = #orc{id=self(), who=Name, x=X, y=Y},
  World ! {New_Orc, polo}, 
  live(World, New_Orc).

live(World, Self) ->
  World ! {Self, polo},
  receive
    {Other, polo} ->
      Dist = distance(Self#orc.x, Self#orc.y, Other#orc.x, Other#orc.y),
      case Dist < 5 of
        true ->
          case Self#orc.target =:= none of
            true ->
  %            react_time(random:uniform(5000)),
              NewSelf = Self#orc{target=Other},
              Other#orc.id ! {NewSelf, {threaten, "You're dead!!"}},
              say(NewSelf#orc.who, Other#orc.who, "You're dead!!"),
              take_action(World, NewSelf);
            false -> 
              case Self#orc.target =:= Other of
                true ->
                  NewSelf = Self#orc{target=Other},
                  take_action(World, NewSelf);
                false ->
                  take_action(World, Self)
              end
          end;
        false ->
          take_action(World, Self)
      end;

    {Other, {attack, Amount}} ->
      io:format("yes"),
      Hp = Self#orc.hit_points - Amount,
      NewSelf = Self#orc{hit_points=Hp},
      case NewSelf#orc.hit_points<1 of
        true ->
          say(NewSelf#orc.who,"himself","Wo is me, for I am dead."),
          NewSelf = Self#orc{alive=false},
          rip(World, NewSelf);
        false ->
          say(NewSelf#orc.who,"himself","Aua!"),
          take_action(World, NewSelf)
      end;

    {Other, {threaten, _}} ->
      %react_time(random:uniform(5000)),
      Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
      %say(Self#orc.who, Other#orc.who, "No You're dead!!!"),
      take_action(World, Self)
  end.

rip(World, Self) ->
  World ! {Self, polo},
  react_time(1000),
  rip(World,Self).

take_action(World, Self) ->
  Target = Self#orc.target,
  case Target =:= none of
    true ->
      move(World, Self, random:uniform(50),random:uniform(50));
    false ->
      case Target#orc.alive of
        true ->
          case distance(Self#orc.x, Self#orc.y, Target#orc.x, Target#orc.y) < 2 of
            true ->
              io:format("~s is Attacking...~n", [Self#orc.who]),
              Target#orc.id ! {Self, {attack, 2}},
              react_time(1200),
              live(World, Self);
            false ->
              move(World, Self, Target#orc.x, Target#orc.y)
          end;
        false ->
          NewSelf = Self#orc{target=none},
          live(World, Self)
      end
  end.

distance(X,Y,Tx,Ty) ->
  DistX = Tx - X,
  DistY = Ty - Y,
  math:sqrt((DistX * DistX) + (DistY * DistY)).

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

