-module(ws_note).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg get notes
msg(M = <<"note/get">>, [Uid]) ->
	List = db_note:get(Uid),
	PList = db_note:to_proplist(db_note:get(Uid)),
	Wrap = db_util:jiffy_wrapper(PList),
	[M, Wrap];

msg(M = <<"note/add">>, [Uid, Text]) ->
	db_note:add(Uid, Text),
	[M, ok];

msg(_,_) -> skip.