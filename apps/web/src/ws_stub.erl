-module(ws_stub).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg ping, returns itself
msg(M = <<"ping">>, []) -> M;

%msg error catch-all
msg(Stub, Args) ->
	?INFO("stub msg:~s args:~p", [<<"no_match_for_message">>, Stub, Args]),
	[stub, ok, Stub, Args].
