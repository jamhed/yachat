-module(ws_todo).
-export([msg/2,to_props/1]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-compile({no_auto_import,[get/1,put/2]}).
-import(db_todo, [put/2, get/1, get/2, get_item/1, add/2, click/2, del/1]).

jiffy_wrapper(List) -> [ {Item} || Item <- List ].

to_props_with_items([H = #todo{id=Tid} | T]) ->
	Items = db_todo:get_items(Tid),
	Props = db_todo:add_tags_prop(db_todo:to_proplist(H)),
	[{Props ++ [{items, jiffy_wrapper(db_todo:to_proplist(Items))}]}] ++ to_props_with_items(T);
to_props_with_items([]) -> [].

to_props([H = #todo{} | T]) ->
	Props = db_todo:add_tags_prop(db_todo:to_proplist(H)),
	[{Props}] ++ to_props(T);
to_props([]) -> [].

%msg create or update todo list
msg(M = <<"todo/update">>, [Uid, Form]) ->
	Plist = db_func:form_to_plist(Form),
	Todo = db_todo:from_proplist(Plist),
	Tags = proplists:get_value(tags, Plist),
	Tid = put(Uid, Todo),
	db_todo:update_tags(Tid, Tags),
	[M, Tid];

%msg get all todo lists with items for user
msg(M = <<"todo/get">>, [Uid]) ->
	[M] ++ [to_props_with_items(get(Uid))];

% get all todo lists without items for user
msg(M = <<"todo/list">>, [Uid]) ->
	[M] ++ [to_props(get(Uid))];

%msg get todo list by id
msg(M = <<"todo/get">>, [Uid, Tid]) ->
	[M] ++ [to_props(get(Uid, Tid))];

%msg add item to todo list
msg(M = <<"todo/add">>, [Uid, Tid, Text]) ->
	[M] ++ [add(get(Uid, Tid), Text)];

%msg get default todo list (check we can write)
msg(M = <<"todo/default">>, [Uid]) ->
	[M] ++ [to_props(db_todo:get_default(Uid))];

msg(M = <<"todo/default">>, [Uid, Tid, State]) ->
	[T] = get(Uid, Tid),
	[M] ++ [db_todo:put(Uid, T#todo{default=State})];

msg(M = <<"todo/move_to">>, [Uid, Tid, MoveTo]) ->
	[T] = get(Uid, Tid),
	[M] ++ [db_todo:put(Uid, T#todo{move_to=MoveTo})];

%msg add item to todo list
msg(M = <<"todo/add">>, [Uid, Text]) ->
	[M] ++ [add(db_todo:get_default(Uid), Text)];

%msg del todo list
msg(M = <<"todo/del">>, [Uid, Tid]) ->
	[M] ++ [del(get(Uid, Tid))];

%msg del or move item from todo list
msg(M = <<"todo/click">>, [Uid, Tid, ItemId]) ->
	[M] ++ [click(get(Uid, Tid), get_item(ItemId))];

%msg get list of todo projects
msg(M = <<"todo/tags">>, [Uid]) ->
	[M] ++ [ Tag || #todo_tag{tag=Tag} <- db_todo:get_tags(Uid) ];

msg(_,_) -> skip.