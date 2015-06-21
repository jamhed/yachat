-module(srv_game).
-include_lib("game/include/game.hrl").
-include_lib("cmon/include/logger.hrl").

-export([init/1, handle_info/2]).
-export([start_link/0]).
-export([game/1]).

-define(TIMEOUT, 10000).
-record(state, {game, timer}).

%% Public API

start_link() -> gen_server:start_link(?MODULE, [], []).
game(Pid) -> Pid ! {state}.

% template functions

init([]) ->
	Timer = erlang:send_after(?TIMEOUT, self(), {timeout}),
	{ok, #state{game=game:gen(), timer=Timer} }.

handle_info({timeout}, S = #state{}) ->
	?INFO("game/timeout: ~p", [self()]),
	Timer = erlang:send_after(?TIMEOUT, self(), {timeout}),
	{noreply, S#state{timer=Timer}};
handle_info(_Info, State) -> ?INFO("info ~p, ~p.", [_Info, State]), {noreply, State}.
