-module(db_attr).
-compile(export_all).
-compile({no_auto_import,[get/1,get/2]}).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").

%XXX: replace to jiffy wrapper
attr_map(AttrList) ->
   [ {[{name, A#user_attr.id}, {value, A#user_attr.value}]} || A <- AttrList ].

get(Uid, Name) ->
   Q = qlc:q([ A || A <- mnesia:table(user_attr), A#user_attr.user_id == Uid, A#user_attr.id == Name ]),
   dbd:do(Q).

list(Uid) -> dbd:index(user_attr, user_id, Uid).

set(Uid, Name, Value) -> dbd:put(#user_attr{ id=Name, value=Value, user_id=Uid }), [ok].

del([#user_attr{id=Id}]) -> dbd:delete(user_attr, Id);
del([]) -> ok.

del(Uid, Name) -> del(get(Uid, Name)).