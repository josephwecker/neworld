-module(server).
-behaviour(gen_server).
-export([start/0]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  %Names = ["Helga", "Toth", "Rorg", "Morteg", "Bgh", "Uuuung", "Elel",
  %  "Dorkr", "Grishnak", "Entlar", "Renry", "Gruk", "Lesli","Artus","Gerni"],
  % my winners were numbers 11, 222, 555, and 666...
  Names = lists:seq(1,900),
  Orcs = [{orc, Orc} || Orc <- lists:map(fun(Name) -> {ok, Pid} = body:birth(Name), Pid
      end, Names)],
  {ok, Orcs}.

handle_call({marco, Orc}, _From, Orcs) ->
  AllTargets = [ {target, A, B, X, Y,
      distance(Orc#orc.x,Orc#orc.y,X,Y), C } ||
      {_,A,B,X,Y,C,_,_,_,_} <- Orcs],
  Targets = [ T || T <- AllTargets, T#target.id =/= Orc#orc.id,
    T#target.distance < 11 ],
  {reply, Targets, Orcs};

handle_call({_Orc, attacks, Target}, _From, Orcs) ->
  % TODO: calculate misses and damages
  Hits = true,
  Damage = 2,
  case Hits of
    true ->
      gen_server:cast(Target, {take_damage, Damage}),
      Reply = "You hit your target!";
    false ->
      gen_server:cast(Target, {message, "You dodged!"}),
      Reply = "You missed!"
  end,
  {reply, Reply, Orcs}.

handle_cast({update, Orc}, Orcs) ->
  NewOrcs = lists:keyreplace(Orc#orc.id, 2, Orcs, Orc),
  {noreply, NewOrcs}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

distance(X,Y,Tx,Ty) ->
  DistX = Tx - X,
  DistY = Ty - Y,
  math:sqrt((DistX * DistX) + (DistY * DistY)).
