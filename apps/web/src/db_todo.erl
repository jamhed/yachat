-module(db_todo).
-compile({no_auto_import,[get/1,put/2]}).
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

from_proplist(Plist) ->
	?INFO("proplist: ~p", [Plist]),
	#todo{
		name = proplists:get_value(name, Plist),
		default = proplists:get_value(default, Plist),
		id = proplists:get_value(id, Plist),
		move_to = proplists:get_value(move_to, lists:reverse(Plist))
	}.

% U = user, Uid
% T = todo, Tid
% TI = todo item

get(Uid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	dbd:do(Q).

get_default(Uid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id, T#todo.default == true
			]),
	dbd:do(Q).

get_item(ItemId) -> dbd:get(todo_item, ItemId).

get_items(Tid) -> lists:reverse(dbd:index(todo_item, todo_id, Tid)).

get(Uid, Tid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid, UT#user_todo.todo_id == Tid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	dbd:do(Q).

clear_default(Uid, #todo{default=true}) ->
	[ put(Uid, T#todo{default=false}) || T <- get_default(Uid) ];

clear_default(_Uid, #todo{}) -> ok.

% update
put(Uid, Todo = #todo{id=Id}) when is_number(Id) ->
	[#todo{}] = get(Uid, Id),
	clear_default(Uid, Todo),
	dbd:put(Todo#todo{stamp=now()});

% create	
put(Uid, Todo = #todo{}) ->
	clear_default(Uid, Todo),
	Id = dbd:make_uid(),
	dbd:put(Todo#todo{stamp=now(), id=Id}),
	dbd:put(#user_todo{user_id=Uid, todo_id=Id, id=dbd:make_uid()}).

add([#todo{id=Tid}], Text) ->
	Id = dbd:make_uid(),
	dbd:put(#todo_item{id=Id, stamp=now(), text=Text, todo_id=Tid});

add(_, _) -> fail.

del([#todo{move_to=MoveTo}], [Item = #todo_item{}]) when is_number(MoveTo) ->
	dbd:put(Item#todo_item{todo_id=MoveTo});

del([#todo{move_to= <<"keep">>}], [#todo_item{}]) -> keep;

del([#todo{}], [#todo_item{id=ItemId}]) ->
	dbd:delete(todo_item, ItemId);

del(_, _) -> fail.

del([#todo{id=Id}]) -> dbd:delete(todo, Id);
del(_) -> fail.
