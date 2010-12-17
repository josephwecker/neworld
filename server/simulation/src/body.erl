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
  X = random:uniform(400),
  Y = random:uniform(400),
  Self = self(),
  mind:awaken(Self),
  NewOrc = #orc{id=self(), who=Name, x=X, y=Y},
  gen_server:cast(server, {update, NewOrc}),
  {ok, NewOrc}.

handle_call(tell_me_everything, _From, Self) ->
  Reply = {Self, gen_server:call(server, {marco, Self})},
  {reply, Reply, Self};

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

%handle_call({Other, {threaten, _}},_From, Self) ->
%  %react_time(random:uniform(5000)),
%  Other#orc.id ! {Self, {threaten_back, "No you're dead!!~n"}},
%  %say(Self#orc.who, Other#orc.who, "No You're dead!!!"),
%  {noreply, Self}.

handle_call({attack, Target}, _From, Self) ->
  io:format("~p attacks ~p~n", [Self#orc.who, Target#target.who]),
  Status = gen_server:call(server, {Self, attacks, Target#target.id}),
  Status,
  Reply = 1400,
  {reply, Reply, Self};

handle_call({move, {X,Y}}, _From, Self) ->
  NewSelf = move(Self, X, Y),
  io:format("(~p) ~p~n", [Self#orc.hit_points, Self#orc.who]),
  %io:format("~p moved from ~p,~p to ~p,~p~n", [Self#orc.who, Self#orc.x,Self#orc.y,NewSelf#orc.x,NewSelf#orc.y]),
  gen_server:cast(server, {update, NewSelf}),
  Reply = 1000,
  {reply, Reply, NewSelf}.

handle_cast({message, _Msg}, Self) ->
  logged,
  {noreply, Self};

handle_cast({here_is_your_mind, Mind}, Self) ->
  NewSelf = Self#orc{mind = Mind},
  {noreply, NewSelf};

handle_cast({take_damage, Amount}, Self) ->
  Hp = Self#orc.hit_points - Amount,
  NewSelf = Self#orc{hit_points=Hp},
  case NewSelf#orc.hit_points<1 of
    true ->
      say(NewSelf#orc.who," ","Wo is me, for I am dead."),
      NewSelf2 = NewSelf#orc{living=false},
      gen_server:cast(Self#orc.mind, die);
    false ->
      NewSelf2 = NewSelf,
      ok
  end,
  gen_server:cast(server, {update, NewSelf2}),
  {noreply, NewSelf2}.

say(From, To, Msg) -> io:format("~p -> ~p: ~p~n",[From, To, Msg]).

move(Self, Target_x, Target_y) ->
  X = new_coords(Self#orc.x, Target_x),
  Y = new_coords(Self#orc.y, Target_y),
  NewSelf = Self#orc{x = X, y = Y},
  NewSelf.

new_coords(O, T) when O == T -> O;
new_coords(O, T) when O < T -> (O + 1);
new_coords(O, T) when O > T -> (O - 1).

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
