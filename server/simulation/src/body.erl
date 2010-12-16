-module(body).
-behaviour(gen_server).
-export([birth/2]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

birth(Name, World) -> gen_server:start_link(?MODULE, [Name, World], []).

init([Name, World]) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  %react_time(random:uniform(4000)),
  X = random:uniform(50),
  Y = random:uniform(30),
  New_Orc = #orc{id=self(), who=Name, x=X, y=Y},
  Self = self(),
  Mind = mind:awaken(),
  {ok, {New_Orc, Mind}}.

handle_call(marco, _From, {Self, Mind}) ->
  io:format("test_2~n"),
  Reply = {Self, polo},
  {reply, Reply, {Self, Mind}};

handle_call({update, NewState}, _From, {_,Mind}) ->
  {noreply, {NewState, Mind}};

handle_call({Other, polo}, _From, {Self,Mind}) ->
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
              gen_server:call(Other#orc.id, {NewSelf, {threaten, "You're dead!!"}}),
              say(NewSelf#orc.who, Other#orc.who, "You're dead!!"),
              NewState = {NewSelf, Mind};
            false ->
              NewState = {Self, Mind}
          end;
        false ->
          case Target#target.id =:= Other#orc.id of
            true ->
              NewSelf = Self#orc{target={target,Other#orc.id,
                                         Other#orc.who,Other#orc.x,
                                         Other#orc.y,Other#orc.living}},
              NewState = {NewSelf, Mind};
            false ->
              NewState = {Self, Mind}
          end
      end;
    false ->
      NewState = {Self, Mind}
  end,
  {noreply, NewState};

handle_call({Other, {attack, Amount}}, _From, {Self, Mind}) ->
  Hp = Self#orc.hit_points - Amount,
  NewSelf = Self#orc{hit_points=Hp},
  case NewSelf#orc.hit_points<1 of
    true ->
      say(NewSelf#orc.who," ","Wo is me, for I am dead."),
      NewestSelf = NewSelf#orc{living=false},
      NewState = {NewestSelf, Mind};
    false ->
      %say(NewSelf#orc.who,"himself","Aua!"),
      NewState = {NewSelf, Mind}
  end,
  {noreply, NewState};

handle_call({Other, {threaten, _}},_From, {Self, Mind}) ->
  %react_time(random:uniform(5000)),
  Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
  %say(Self#orc.who, Other#orc.who, "No You're dead!!!"),
  {noreply, {Self, Mind}}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

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
      
%% Functions to make
%move(Mind, Self, random:uniform(50),random:uniform(50));
%
              %io:format("~s attacks ~s~n", [Self#orc.who, Target#target.who]),
              %Target#target.id ! {Self, {attack, 2}},
              %react_time(random:uniform(400)),
%
%
%
move(Mind, OldSelf, Target_x, Target_y) ->
  X = new_coords(OldSelf#orc.x, Target_x),
  Y = new_coords(OldSelf#orc.y, Target_y),
  Self = OldSelf#orc{x = X, y = Y},
  gen_server:call(Mind,{decide,{}}).

new_coords(O, T) when O == T -> O;
new_coords(O, T) when O < T -> (O + 1);
new_coords(O, T) when O > T -> (O - 1).

