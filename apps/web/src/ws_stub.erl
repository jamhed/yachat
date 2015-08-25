-module(ws_stub).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg ping, returns itself
msg(M = <<"ping">>, []) -> M;

%msg error catch-all
msg(Stub, Args) ->
	?INFO("STUB msg:~s args:~p", [Stub, Args]),
	[<<"no_match_for_message">>, fail, Stub, Args].
