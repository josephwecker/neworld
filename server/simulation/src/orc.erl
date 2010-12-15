-module(orc).
-export([birth/2]).
-include("orc.hrl").

birth(Name, World) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  react_time(random:uniform(1000)),
  X = random:uniform(50),
  Y = random:uniform(30),
  New_Orc = #orc{id=self(), who=Name, x=X, y=Y},
  World ! {New_Orc, polo},
  Self = self(),
  spawn(fun() -> act(Self) end),
  live(World, New_Orc).

live(World, Self) ->
  receive
    {Body, get_self} ->
      Body ! {self(), {your_stats,Self}},
      World ! {Self, polo},
      live(World, Self);

    {Body, {update_self, NewSelf}} ->
      live(World, NewSelf);

    {Other, polo} ->
      Dist = distance(Self#orc.x, Self#orc.y, Other#orc.x, Other#orc.y),
      case Dist < 8 of
        true ->
          Target = Self#orc.target,
          case Target =:= none of
            true ->
              case Other#orc.living of
                true ->
                  NewSelf = Self#orc{target={target,Other#orc.id,
                                             Other#orc.who,Other#orc.x,
                                             Other#orc.y,Other#orc.living}},
                  Other#orc.id ! {NewSelf, {threaten, "You're dead!!"}},
                  say(NewSelf#orc.who, Other#orc.who, "You're dead!!"),
                  live(World, NewSelf);
                false ->
                  live(World, Self)
              end;
            false ->
              case Target#target.id =:= Other#orc.id of
                true ->
                  NewSelf = Self#orc{target={target,Other#orc.id,
                                             Other#orc.who,Other#orc.x,
                                             Other#orc.y,Other#orc.living}},
                  live(World, NewSelf);
                false ->
                  live(World, Self)
              end
          end;
        false ->
          live(World, Self)
      end;

    {Other, {attack, Amount}} ->
      Hp = Self#orc.hit_points - Amount,
      NewSelf = Self#orc{hit_points=Hp},
      case NewSelf#orc.hit_points<1 of
        true ->
          say(NewSelf#orc.who," ","Wo is me, for I am dead."),
          NewestSelf = NewSelf#orc{living=false},
          rip(World, NewestSelf);
        false ->
          %say(NewSelf#orc.who,"himself","Aua!"),
          live(World, NewSelf)
      end;

    {Other, {threaten, _}} ->
      %react_time(random:uniform(5000)),
      Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
      %say(Self#orc.who, Other#orc.who, "No You're dead!!!"),
      live(World, Self)
  end.

rip(World, Self) ->
  World ! {Self, polo},
  react_time(1000),
  rip(World,Self).

act(Observer) ->
  react_time(1000),
  Observer ! {self(),get_self},
  receive
    {Observer, {your_stats, Self}} ->
      take_action(Observer, Self)
  end.


take_action(Mind, Self) ->
  Target = Self#orc.target,
  case Target =:= none of
    true ->
      move(Mind, Self, random:uniform(50),random:uniform(50));
    false ->
      case Target#target.living of
        true ->
          case distance(Self#orc.x, Self#orc.y, Target#target.x, Target#target.y) < 2 of
            true ->
              %io:format("~s attacks ~s~n", [Self#orc.who, Target#target.who]),
              Target#target.id ! {Self, {attack, 2}},
              react_time(200),
              act(Mind);
            false ->
              move(Mind, Self, Target#target.x, Target#target.y)
          end;
        false ->
          NewSelf = Self#orc{target=none},
          Mind ! {self(),{update_self,NewSelf}},
          act(Mind)
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

move(Mind, OldSelf, Target_x, Target_y) ->
  X = new_coords(OldSelf#orc.x, Target_x),
  Y = new_coords(OldSelf#orc.y, Target_y),
  Self = OldSelf#orc{x = X, y = Y},
  Mind ! {self(),{update_self,Self}},
  act(Mind).

new_coords(O, T) when O == T -> O;
new_coords(O, T) when O < T -> (O + 1);
new_coords(O, T) when O > T -> (O - 1).

