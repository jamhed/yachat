-module(db_user).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-define(SYSTEM,1).

get(Id) when is_number(Id) -> dbd:get(user, Id).

to_proplist(#user{} = U) -> lists:zip(record_info(fields, user), to_list(U)).
to_list(U) -> [ map_field(F) || F <- tl(tuple_to_list(U)) ].

filter_props(Props, List) -> [ { F, proplists:get_value(F, Props) } || F <- List ].

extend_with_props(User, [{Name,Value} | Props]) -> extend_with_props(  User ++ [{Name,Value}], Props );
extend_with_props(User, []) -> User.

map_field(undefined) -> null;
map_field(Now = {_,_,_}) -> cvt:now_to_binary(Now);
map_field(F) -> F.

enum_f() -> 
   Fields = record_info(fields, user),
   FNums  = lists:zip(Fields, lists:seq(2,length(Fields)+1)),
   FNums.

set_by_name(U, Name, Value) -> 
   PList = enum_f(),
   case proplists:get_value(Name, PList) of
      X when is_number(X) -> UX = setelement(X, U, Value);
      _ -> UX = U
   end,
   UX.

set_by_name(U, []) -> U;
set_by_name(U, [Name, Value | Rest ]) ->
   NameAtom = erlang:binary_to_atom(Name, utf8),
   set_by_name( set_by_name(U, NameAtom, Value), Rest ).

set_by_props(U, []) -> U;
set_by_props(U, [{Name,Value} | Rest]) ->
   NameAtom = erlang:binary_to_atom(Name, utf8),
   set_by_props( set_by_name(U, NameAtom, Value), Rest ).
    
user_to_props_short([U]) -> filter_props( to_proplist(U), [id,username,email] );
user_to_props_short(_) -> [].

add_file(Plist, [{Name, Id}]) -> Plist ++ [{Name,Id}];
add_file(Plist, []) -> Plist.

user_to_props([U]) ->
   Plist = to_proplist(U),
   proplists:delete(password, Plist),
   add_file(Plist, db_file:by_type(U#user.id, <<"avatar">>));
user_to_props(_) -> [].

detail(List) when is_list(List) -> [ detail(Uid) || Uid <- List ];
detail(Uid) -> {user_to_props( get(Uid) )}.

detail_short(List) when is_list(List)  -> [detail_short(Uid) || Uid <- List];
detail_short(Uid) -> {user_to_props_short( get(Uid) )}.

% id of convs user is in
conv(Uid) ->
	Q = qlc:q([ C#user_conv.conv_id || C <- mnesia:table(user_conv), C#user_conv.user_id == Uid ]),
	dbd:do(Q).

files(Uid) ->
	Q = qlc:q([ [C#user_file.id,C#user_file.type,C#user_file.mime] || C <- mnesia:table(user_file), C#user_file.user_id == Uid ]),
	dbd:do(Q).

files(Uid, Type) ->
	Q = qlc:q([ [C#user_file.id, C#user_file.mime] || C <- mnesia:table(user_file),
      C#user_file.user_id == Uid, C#user_file.type == Type ]),
	dbd:do(Q).

file_delete(_Uid, FileId) -> dbd:delete(user_file, FileId).

list_online(_) ->
	Q = qlc:q([ detail(C#user_online.user_id) || C <- mnesia:table(user_online) ]),
	dbd:do(Q).

pids([H | T]) -> [ pids(H) ] ++ [ pids(Uid) || Uid <- T ];
pids(Uid) ->
	Q = qlc:q([ U#user_online.pid || U <- mnesia:table(user_online), U#user_online.user_id == Uid ]),
	dbd:do(Q).

sid_to_uid(Sid) ->
   case dbd:index(user_online, session_id, Sid) of
      [UO] -> UO#user_online.user_id;
      _    -> fail
   end.

attr_get(Uid, Name) ->
   Q = qlc:q([ {A#user_attr.id, A#user_attr.value} || A <- mnesia:table(user_attr),
      A#user_attr.user_id == Uid, A#user_attr.id == Name ]),
   dbd:do(Q).

attr_get_all(Uid) -> [ {UA#user_attr.id, UA#user_attr.value} || UA <- dbd:index(user_attr, user_id, Uid) ].


attr_set(Uid, Name, Value) -> dbd:put(#user_attr{ id=Name, value=Value, user_id=Uid }), [ok].


get_by_fb(Id) when is_binary(Id) -> dbd:index(user, facebook_id, Id);
get_by_fb(_) -> [].
get_by_email(Id) when is_binary(Id) -> dbd:index(user, email, Id);
get_by_email(_) -> [].

clear_online([]) -> ok;
clear_online([#user_online{id=Id} | T]) -> dbd:delete(user_online, Id), clear_online(T).

drop_online_status([]) -> ok;
drop_online_status([#user_online{id=Id, pid=Pid, user_id=Uid} | R]) ->
   ?INFO("drop_online_status: id=~p pid=~p", [Id, Pid]),
   notify_conv(Uid, conv(Uid), [<<"offline">>, detail_short(Uid)]),
   dbd:delete(user_online, Id),
   drop_online_status(R).

drop_online_status_k([]) -> ok;
drop_online_status_k([#user_online{id=Id, pid=Pid, user_id=Uid} | R]) ->
   ?INFO("drop_online_status_k: id=~p pid=~p", [Id, Pid]),
   notify_conv(Uid, conv(Uid), [<<"offline">>, detail_short(Uid)]),
   drop_online_status_k(R).

offline(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status_k(R).

logout(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status(R).

notify_conv(_Uid, [], _Msg) -> ok;
notify_conv(Uid, [Cid | Rest], Msg) ->
   db_conv:notify(Uid, Cid, Msg),
   notify_conv(Uid, Rest, Msg).

get_online_status(Uid) ->
	Q = qlc:q([ C || C <- mnesia:table(user_online), C#user_online.user_id == Uid ]),
	dbd:do(Q).

handle_online_status([], Uid, Pid) ->
   ?INFO("user_online: user_id=~p pid=~p", [Uid, Pid]),
   Sid = dbd:make_uid(),
   dbd:put(#user_online{
      id=dbd:next_id(user_online),
      stamp=now(),
      pid=Pid,
      user_id=Uid,
      session_id=Sid}),
   notify_conv(Uid, conv(Uid), [<<"online">>, db_user:detail_short(Uid)]),
   Sid;

handle_online_status([UO], Uid, Pid) ->
   ?INFO("user_online: already online, skip: user_id=~p pid=~p", [Uid, Pid]),
   dbd:put(UO#user_online{pid=Pid}),
   UO#user_online.session_id.

online(Uid) -> handle_online_status(get_online_status(Uid),Uid,self()).
