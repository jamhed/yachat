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
		id = proplists:get_value(id, Plist),
		move_to = proplists:get_value(move_to, lists:reverse(Plist)),
		prio = proplists:get_value(prio, Plist)
	}.

% U = user, Uid
% T = todo, Tid
% TI = todo item

% sort_todo(#todo{prio=prioA}, #todo{prio=prioB}) -> true.

get(Uid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	lists:sort(fun(#todo{prio=A},#todo{prio=B}) -> A < B end, dbd:do(Q)).

check_tag(Tid, Tag) ->
	Q = qlc:q([ T || T <- mnesia:table(todo_tag), T#todo_tag.todo_id == Tid, T#todo_tag.tag == Tag ]),
	dbd:do(Q).

check_default(Tid, Tag) ->
	Q = qlc:q([ T || T <- mnesia:table(todo_tag),
		T#todo_tag.todo_id == Tid,
		T#todo_tag.tag == Tag,
		T#todo_tag.default == true
	]),
	dbd:do(Q).

by_tag(Uid, Tag) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	L = lists:sort(fun(#todo{prio=A},#todo{prio=B}) -> A < B end, dbd:do(Q)),
	lists:filter(fun(#todo{id=Id}) -> db_util:to_bool(check_tag(Id,Tag)) end, L).

get_by_tag(Uid, [Tag]) -> by_tag(Uid, Tag);
get_by_tag(Uid, []) -> get(Uid).

get_default(Uid, [Tag]) ->
	lists:filter(fun(#todo{id=Id}) -> db_util:to_bool(check_default(Id,Tag)) end, by_tag(Uid, Tag)).

clear_default(Uid, [Tag]) ->
	[ set_tag(Tid, Tag, false) || #todo{id=Tid} <- get_default(Uid, [Tag]) ].

get_item(ItemId) -> dbd:get(todo_item, ItemId).

get_items(Tid) -> lists:sort(fun(#todo_item{stamp=A},#todo_item{stamp=B}) -> A>B end, dbd:index(todo_item, todo_id, Tid)).

get(Uid, Tid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid, UT#user_todo.todo_id == Tid,
			T <- mnesia:table(todo), T#todo.id == UT#user_todo.todo_id
			]),
	dbd:do(Q).


% update
put(Uid, Todo = #todo{id=Id}) when is_number(Id) ->
	[#todo{}] = get(Uid, Id),
	dbd:put(Todo#todo{stamp=now()}),
	Id;

% create	
put(Uid, Todo = #todo{}) ->
	Id = dbd:make_uid(),
	dbd:put(Todo#todo{stamp=now(), id=Id}),
	dbd:put(#user_todo{user_id=Uid, todo_id=Id, id=dbd:make_uid()}),
	Id.

add([#todo{id=Tid}], Text) when is_binary(Text), byte_size(Text)>0 ->
	Id = dbd:make_uid(),
	dbd:put(#todo_item{id=Id, stamp=now(), text=Text, todo_id=Tid}),
	Tid;

add(_, Text) when is_binary(Text), byte_size(Text)==0 -> empty;

add(_, _) -> fail.

click([#todo{move_to=MoveTo}], [Item = #todo_item{}]) when is_number(MoveTo) ->
	dbd:put(Item#todo_item{todo_id=MoveTo, stamp=now()});

click([#todo{move_to= <<"keep">>}], [Item = #todo_item{}]) ->
	dbd:put(Item#todo_item{stamp=now()});

click([#todo{}], [#todo_item{id=ItemId}]) ->
	dbd:delete(todo_item, ItemId);

click(_, _) -> fail.

del([#todo{id=Id}]) -> dbd:delete(todo, Id);
del(_) -> fail.

get_unique_tags(Uid) ->
	TagList = [ Tag || #todo_tag{tag=Tag} <- get_all_tags(Uid) ],
	lists:usort(TagList).

get_all_tags(Uid) ->
	Q = qlc:q([ T ||
			UT <- mnesia:table(user_todo), UT#user_todo.user_id == Uid,
			T <- mnesia:table(todo_tag), T#todo_tag.todo_id == UT#user_todo.todo_id
		]),
	dbd:do(Q).

get_tags(Tid) ->
	Q = qlc:q([ T || T <- mnesia:table(todo_tag), T#todo_tag.todo_id == Tid	]),
	dbd:do(Q).

delete_tags(Tid) ->
	[ dbd:delete(todo_tag, Id) || #todo_tag{id=Id} <- get_tags(Tid) ].

set_tag(_Tid, <<"">>) -> ok;
set_tag(Tid, Tag) -> set_tag(Tid, Tag, false).

set_tag(Tid, [Tag], Default) -> set_tag(Tid, Tag, Default);
set_tag(_Tid, [], _Default) -> ok;
set_tag(Tid, Tag, Default) -> dbd:put(#todo_tag{id={Tid,Tag}, todo_id=Tid, tag=Tag, default=Default}).


update_tags(Tid, Tags) ->
	TagList = binary:split(Tags, <<" ">>, [global]),
	delete_tags(Tid),
	[ set_tag(Tid, Tag) || Tag <- TagList ].

join_binary(A, B) when bit_size(B) > 0 -> join_binary(A, B, <<" ">>);
join_binary(A, _) -> A.
join_binary(A, B, S) -> <<A/binary, S/binary, B/binary>>. 

tags_as_binary(Tid) ->
	lists:foldr(fun join_binary/2, <<"">>, [ Tag || #todo_tag{tag=Tag} <- get_tags(Tid) ]).

add_tags_prop(P) ->
	Tid = proplists:get_value(id, P),
	P ++ [{tags, tags_as_binary(Tid)}].