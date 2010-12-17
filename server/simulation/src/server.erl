-module(server).
-behaviour(gen_server).
-export([start/0]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  timer:apply_interval(250, gen_server, cast, [server, ping]).

init([]) ->
  Names = ["Helga", "Toth", "Rorg", "Morteg", "Bgh", "Uuuung", "Elel", "Dorkr"],
  Orcs = lists:map(fun(Name) -> {ok, Pid} = body:birth(Name), Pid end, Names),
  {ok, {[], Orcs}}.

  %"Grishnak", "Entlar", "Renry", "Gruk", "Lesli","Artus","Gerni"],
handle_call({Self, marco}, _From, {Posits, Orcs}) ->
  Targets = [ {target, A, B, X, Y, distance(Self#orc.x,Self#orc.y,X,Y), C} ||
      {_,A,B,X,Y,C,_,_,_} <- Posits],
  {reply, Targets, {Posits, Orcs}}.

handle_cast(ping, {_, Orcs}) ->
  io:format("..peep..~n"),
  NewPosits = lists:map(fun(Orc) ->
        Posit = gen_server:call(Orc, marco),
        Posit
    end, Orcs),
  {noreply, {NewPosits, Orcs}};
 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

distance(X,Y,Tx,Ty) ->
  DistX = Tx - X,
  DistY = Ty - Y,
  math:sqrt((DistX * DistX) + (DistY * DistY)).
