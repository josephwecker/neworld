-module(server).
-behaviour(gen_server).
-export([start/0, ping/0]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
ping() -> gen_server:multi_call(body, start).

init([]) ->
  Names = ["Helga", "Toth", "Rorg", "Morteg", "Bgh", "Uuuung", "Elel", "Dorkr",
  "Grishnak", "Entlar", "Renry", "Gruk", "Les","Artus","Gerni"],
  Self = self(),
  Orcs = lists:map(fun(Name) -> spawn(fun() -> body:birth(Name, Self) end) end, Names),
  io:format("~ntest_1~n"),
  {ok, Orcs}.

handle_call({{Orc, polo}}, _From, Orcs) ->
  io:format("~ntest_0~n"),

  io:format("(~p)~s: ~p,~p~n",[Orc#orc.hit_points,Orc#orc.who,Orc#orc.x,Orc#orc.y]),
  lists:foreach(fun(Neighbor) ->
    case Neighbor =:= Orc#orc.id of
      true -> do_nothing;
      false -> Neighbor ! {Orc, polo}
    end
  end, Orcs),
  {noreply, Orcs}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.
