-module(ws_todo_tag).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg get list of todo tags
msg(M = <<"todo/tags">>, [Uid]) ->
	[M, db_todo:get_default_tag(Uid)]
		++ [lists:usort([ Tag || #todo_tag{tag=Tag} <- db_todo:get_all_tags(Uid) ])];

%msg set current tag
msg(M = <<"todo/tag/set">>, [Uid, Tag]) ->
	db_todo:set_default_tag(Uid, Tag),
	[M, ok];

%msg get current tag
msg(M = <<"todo/tag/current">>, [Uid]) ->
	Tag = db_todo:get_default_tag(Uid),
	[M, Tag];

msg(_,_) -> skip.