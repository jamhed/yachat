-module(srv_cleanup).
-include_lib("cmon/include/config.hrl").
-include_lib("cmon/include/logger.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("web/include/db.hrl").

-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECK, ?CFG(check_interval_ms, 10000)).
-define(STALE, ?CFG(stale_timeout_ms, 60000)).

%% Public API

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

% template func

init([]) ->
	Timer = erlang:send_after(1, self(), check),
	{ok, Timer}.

handle_call(stop, _From, State) ->
	?INFO("stopping by ~p, state was ~p.", [_From, State]),
	{stop, normal, stopped, State};
handle_call(state, _From, State) ->
	?INFO("~p is asking for the state.", [_From]),
	{reply, State, State};
handle_call(_Request, _From, State) ->
	?INFO("call ~p, ~p, ~p.", [_Request, _From, State]),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	?INFO("cast ~p, ~p.", [_Msg, State]),
	{noreply, State}.

handle_info(check, OldTimer) ->
	erlang:cancel_timer(OldTimer),
	clear_stale_sessions(),
	Timer = erlang:send_after(?CHECK, self(), check),
	{noreply, Timer};
handle_info(_Info, State) ->
	?INFO("info ~p, ~p.", [_Info, State]),
 	{noreply, State}.

terminate(_Reason, _State) ->
	?INFO("terminate ~p, ~p", [_Reason, _State]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	?INFO("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
	{ok, State}.

%payload

now_to_seconds({Mega, Sec, Micro}) -> Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

get_stale_sessions() ->
	Now = now_to_seconds(now()),
	Q = qlc:q([ UO#user_online.id || UO <- mnesia:table(user_online),
    	Now - now_to_seconds(UO#user_online.stamp) > ?STALE,
    	UO#user_online.online == false
    ]),
	dbd:do(Q).

delete_sessions([Id | Rest]) ->
	?INFO("Purge session: ~p", [Id]),
	dbd:delete(user_online, Id),
	delete_sessions(Rest);
delete_sessions([]) -> ok.

clear_stale_sessions() -> delete_sessions(get_stale_sessions()).