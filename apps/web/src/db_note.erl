-module(db_note).
-compile({no_auto_import,[get/1, get/2, put/2]}).
-compile(export_all).
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-include_lib("db/include/db_macro.hrl").

%to_proplist(#note{})
?TO_PROPS(user_note);
to_proplist([H | T]) -> [to_proplist(H)] ++ to_proplist(T);
to_proplist([]) -> [].

stamp_sort(#user_note{stamp=A},#user_note{stamp=B}) -> A > B.

get(Uid) -> get(Uid, 20).

get(Uid, Limit) ->
	Q = qlc:q([ N || N <- mnesia:table(user_note), N#user_note.user_id == Uid ]),
	lists:sort(fun stamp_sort/2, dbd:limit(Q, Limit)).

add(Uid, Text) ->
	dbd:put(#user_note{id=dbd:make_uid(), stamp=now(), user_id=Uid, text=Text}).