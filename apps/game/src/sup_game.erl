-module(sup_game).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	Child = {srv_game,
				{srv_game, start_link, []},
				permanent, 2000, worker, [mychild]},
  	{ok, {{one_for_all, 1, 1}, [Child]}}.