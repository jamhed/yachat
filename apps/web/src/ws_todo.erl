-module(ws_todo).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-compile({no_auto_import,[get/1,put/2]}).
-import(db_todo, [put/2, get/1, get/2, get_item/1, add/2, click/2, del/1]).

%msg create or update todo list
msg(M = <<"todo/update">>, [Uid, Form]) ->
	Plist = db_func:form_to_plist(Form),
	Todo = db_todo:from_proplist(Plist),
	Tags = proplists:get_value(tags, Plist),
	Tid = put(Uid, Todo#todo{move_to= <<"keep">>}),
	db_todo:update_tags(Tid, Tags),
	db_todo:set_default_tag(Uid),
	[M, Tid];

%msg get all todo lists with items for user
msg(M = <<"todo/get">>, [Uid]) ->
	Tag = db_todo:get_default_tag(Uid),
	[M] ++ [db_todo:to_props_jiffy(db_todo:by_tag(Uid, Tag), [tags, items])];

% get all todo lists without items for user
msg(M = <<"todo/list">>, [Uid]) ->
	Tag = db_todo:get_default_tag(Uid),
	[M
	, db_todo:to_props_jiffy(db_todo:by_tag(Uid, Tag))
	, db_todo:to_props_jiffy(db_todo:get_default_todo(Uid, Tag))];

%msg get todo list by id
msg(M = <<"todo/load">>, [Uid, Tid]) ->
	[M] ++ [db_todo:to_props_jiffy(get(Uid, Tid))];

%msg add item to default todo list
msg(M = <<"todo/add">>, [Uid, Text]) ->
	Tag = db_todo:get_default_tag(Uid),
	[M] ++ [add(db_todo:get_default_todo(Uid, Tag), Text)];

%msg get default todo (check we can write)
msg(M = <<"todo/default">>, [Uid]) ->
	Tag = db_todo:get_default_tag(Uid),
	[M, db_todo:to_props_jiffy(db_todo:get_default_todo(Uid, Tag))];

%msg set default todo
msg(M = <<"todo/default">>, [Uid, Tid]) ->
	Tag = db_todo:get_default_tag(Uid),
	db_todo:set_default_todo(Uid, Tid, Tag),
	[M, ok];

%msg move item to another todo list
msg(M = <<"todo/move_to">>, [Uid, Tid, MoveTo]) ->
	[T] = get(Uid, Tid),
	[M] ++ [db_todo:put(Uid, T#todo{move_to=MoveTo})];

%msg del todo list
msg(M = <<"todo/del">>, [Uid, Tid]) ->
	[M] ++ [del(get(Uid, Tid))];

%msg del or move item from todo list
msg(M = <<"todo/click">>, [Uid, Tid, ItemId]) ->
	[M] ++ [click(get(Uid, Tid), get_item(ItemId))];

msg(M = <<"todo/export">>, [Uid]) -> [M, db_todo:export(Uid)];

msg(_,_) -> skip.