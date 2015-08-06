-module(db_todo).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-include_lib("db/include/db_macro.hrl").

%to_proplist(#todo{})
?TO_PROPS(todo);
%to_proplist(#todo_item{})
?TO_PROPS(todo_item);
%list processor
to_proplist([H | T]) -> [to_proplist(H)] ++ to_proplist(T);
to_proplist([]) -> [].

% U = user, Uid
% T = todo, Tid
% TI = todo item

get(Uid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	dbd:do(Q).

get_item(ItemId) -> dbd:get(todo_item, ItemId).

get_items(Tid) -> dbd:index(todo_item, todo_id, Tid).

get(Uid, Tid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid, UT#user_todo.todo_id == Tid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	dbd:do(Q).

new(Uid, Name) ->
	Id = dbd:make_uid(),
	dbd:put(#todo{id=Id, stamp=now(), name=Name}),
	dbd:put(#user_todo{user_id=Uid, todo_id=Id, id=dbd:make_uid()}).

add([#todo{id=Tid}], Text) ->
	Id = dbd:make_uid(),
	dbd:put(#todo_item{id=Id, stamp=now(), text=Text, todo_id=Tid});

add(_, _) -> fail.

del([#todo{move_to=MoveTo}], [#todo_item{id=ItemId}]) when is_number(MoveTo) ->
	dbd:put(#todo_item{id=ItemId, todo_id=MoveTo});

del([#todo{move_to=_}], [#todo_item{id=ItemId}]) ->
	dbd:delete(todo_item, ItemId);

del(_, _) -> fail.

