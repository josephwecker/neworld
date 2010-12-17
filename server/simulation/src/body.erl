-module(body).
-behaviour(gen_server).
-export([birth/1]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

birth(Name) -> gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  %react_time(random:uniform(4000)),
  X = random:uniform(50),
  Y = random:uniform(50),
  New_Orc = #orc{id=self(), who=Name, x=X, y=Y},
  Self = self(),
  mind:awaken(Self),
  {ok, New_Orc}.

handle_call(marco, _From, Self) ->
  Reply = Self,
  {reply, Reply, Self};

handle_call(tell_me_everything, _From, Self) ->
  Reply = {Self, gen_server:call(server, {Self, marco})},
  {reply, Reply, Self}.

%handle_call({Other, polo}, _From, Self) ->
%  Dist = server:distance(Self#orc.x, Self#orc.y, Other#orc.x, Other#orc.y),
%  case Dist < 8 of
%    true ->
%      Target = Self#orc.target,
%      case Target =:= none of
%        true ->
%          case Other#orc.living of
%            true ->
%              NewSelf = Self#orc{target={target,Other#orc.id,
%                                         Other#orc.who,Other#orc.x,
%                                         Other#orc.y,Other#orc.living}},
%              gen_server:call(Other#orc.id, {NewSelf, {threaten, "You're dead!!"}}),
%              say(NewSelf#orc.who, Other#orc.who, "You're dead!!");
%            false ->
%              NewSelf = Self
%          end;
%        false ->
%          case Target#target.id =:= Other#orc.id of
%            true ->
%              NewSelf = Self#orc{target={target,Other#orc.id,
%                                         Other#orc.who,Other#orc.x,
%                                         Other#orc.y,Other#orc.living}};
%            false ->
%              NewSelf = Self
%          end
%      end;
%    false ->
%      NewSelf = Self
%  end,
%  {noreply, NewSelf};

%handle_call({Other, {attack, Amount}}, _From, Self) ->
%  Hp = Self#orc.hit_points - Amount,
%  NewSelf = Self#orc{hit_points=Hp},
%  case NewSelf#orc.hit_points<1 of
%    true ->
%      say(NewSelf#orc.who," ","Wo is me, for I am dead."),
%      NewestSelf = NewSelf#orc{living=false};
%    false ->
%      %say(NewSelf#orc.who,"himself","Aua!"),
%      NewestSelf = NewSelf,
%      ok
%  end,
%  {noreply, NewestSelf};

%handle_call({Other, {threaten, _}},_From, Self) ->
%  %react_time(random:uniform(5000)),
%  Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
%  %say(Self#orc.who, Other#orc.who, "No You're dead!!!"),
%  {noreply, Self}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%say(From, To, Msg) ->
%  io:format("~s -> ~s: ~s~n",[From, To, Msg]).

%react_time(Time) ->
%  receive
%    after Time ->
%      true
%  end.
      
%% Functions to make
%move(Mind, Self, random:uniform(50),random:uniform(50));
%
              %io:format("~s attacks ~s~n", [Self#orc.who, Target#target.who]),
              %Target#target.id ! {Self, {attack, 2}},
              %react_time(random:uniform(400)),
%
%
%
%move(Mind, OldSelf, Target_x, Target_y) ->
%  X = new_coords(OldSelf#orc.x, Target_x),
%  Y = new_coords(OldSelf#orc.y, Target_y),
%  Self = OldSelf#orc{x = X, y = Y},
%  gen_server:call(Mind,{decide,{}}).

%new_coords(O, T) when O == T -> O;
%new_coords(O, T) when O < T -> (O + 1);
%new_coords(O, T) when O > T -> (O - 1).

