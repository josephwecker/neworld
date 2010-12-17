-module(mind).
-behaviour(gen_server).
-export([awaken/1]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

awaken(Body) ->
  {ok, Mind} = gen_server:start_link(?MODULE, [Body], []),
  %gen_server:cast(Mind, think).
  timer:apply_interval(2000, gen_server, cast, [Mind, think]).

init([Body]) ->
  Mind = #sinn{},
  {ok, {Mind, Body}}.

handle_call({decide, Self}, _From, State) -> Reply = yes, {reply, Reply, State}.
  

handle_cast(think, {Mind, Body}) ->
  {Self, Others} = gen_server:call(Body, tell_me_everything),
  io:format("Received Targets~n"),
  Target = Mind#sinn.target,
  case Target =:= none of
    true ->
      Reply = {move, random};
    false ->
      case Target#target.living of
        true ->
          case body:distance(Self#orc.x, Self#orc.y, Target#target.x, Target#target.y) < 2 of
            true ->
              Reply = {attack};
            false ->
              Reply = {move, target}
          end;
        false ->
          Reply = {drop_target}
      end
  end,
  gen_server:cast(Body, Reply),
  {noreply, {Mind, Body}};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
