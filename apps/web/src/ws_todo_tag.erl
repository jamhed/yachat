-module(ws_todo_tag).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg get list of todo tags
msg(M = <<"todo/tags">>, [Uid, Sid]) ->
	[M, db_session:get(Sid, tag)]
		++ [lists:usort([ Tag || #todo_tag{tag=Tag} <- db_todo:get_all_tags(Uid) ])];

%msg set current tag
msg(M = <<"todo/tag/set">>, [_Uid, Sid, Tag]) ->
	db_session:set(Sid, tag, Tag),
	[M, ok];

%msg clear current tag
msg(M = <<"todo/tag/clear">>, [_Uid, Sid]) ->
	db_session:del(Sid, tag),
	[M, ok];

%msg get current tag
msg(M = <<"todo/tag/current">>, [_Uid, Sid]) ->
	Tag = db_session:get(Sid, tag),
	[M, Tag];

msg(_,_) -> skip.