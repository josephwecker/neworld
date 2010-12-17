-module(mind).
-behaviour(gen_server).
-export([awaken/1]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

awaken(Body) ->
  {ok, Mind} = gen_server:start_link(?MODULE, [Body], []),
  timer:apply_after(1000, gen_server, cast, [Body, {here_is_your_mind, Mind}]),
  timer:apply_after(1000, gen_server, cast, [Mind, think]).

init([Body]) ->
  Self = #sinn{body=Body},
  {ok, Self}.

% TODO:
handle_call({decide, _Body}, _From, Self) -> Reply = yes, {reply, Reply, Self}.
  

handle_cast(think, Self) ->
  {Body, Others} = gen_server:call(Self#sinn.body, tell_me_everything),
  Memory = update_memory(Self#sinn.memory, Others),
  NewSelf = Self#sinn{memory = Memory},
  Targets = NewSelf#sinn.memory,
  %io:format("~p: ~p~n",[Body#orc.who, Targets]),
  Target = choose_target(Targets),
  case Target =:= none of
    true ->
      Command = {move, {
          Body#orc.x + random:uniform(3)-2,
          Body#orc.y + random:uniform(3)-2}};
    false ->
  %io:format("~p targets ~p~n",[Body#orc.who, Target#target.who]),
      case Target#target.distance < 2 of
        true ->
          Command = {attack, Target};
        false ->
          Command = {move, {Target#target.x, Target#target.y}}
      end
  end,
  Delay = gen_server:call(NewSelf#sinn.body, Command),
  timer:apply_after(Delay, gen_server, cast, [self(), think]),
  {noreply, NewSelf};

handle_cast(die, Self) -> {stop, normal, Self}.

choose_target([H|T]) ->
  case H#target.living of
    true -> choose_target(T, H);
    false -> choose_target(T)
  end;
choose_target([]) -> none.
choose_target([H|T], Target) ->
  case H#target.living of
    true ->
      case H#target.distance < Target#target.distance of
        true ->  NewTarget = H;
        false -> NewTarget = Target
      end;
    false -> NewTarget = Target
  end,
  choose_target(T, NewTarget);
choose_target([], Target) -> Target.

update_memory(List, [H|T]) ->
  case lists:keymember(element(2,H),2,List) of
    true -> NewList = lists:keyreplace(element(2,H),2,List,H);
    false -> NewList = lists:append(List, [H])
  end,
  update_memory(NewList, T);
update_memory(List, []) -> List.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
