-module(mind).
-behaviour(gen_server).
-export([awaken/0]).
-include("records.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

awaken() -> gen_server:start_link(?MODULE, [], []).

%other function calls
%do(Action) -> gen_server:call(?MODULE,{do, Action}).


% Call handlers:
init([]) ->
  io:format("test_3"),
  {ok, good}.

handle_call({decide, Self}, _From, State) ->
  Target = Self#orc.target,
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
  {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.
