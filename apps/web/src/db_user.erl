-module(db_user).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-include_lib("db/include/db_macro.hrl").
-define(SYSTEM,1).

?TO_PROPS(user).

get(Id) when is_number(Id) -> dbd:get(user, Id).

filter_props(Props, List) -> [ { F, proplists:get_value(F, Props) } || F <- List ].

extend_with_props(User, [{Name,Value} | Props]) -> extend_with_props(  User ++ [{Name,Value}], Props );
extend_with_props(User, []) -> User.

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

user_to_props([U]) -> proplists:delete(password, to_proplist(U));
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
	Q = qlc:q([ map_user_file([C]) || C <- mnesia:table(user_file), C#user_file.user_id == Uid ]),
	dbd:do(Q).

files(Uid, Type) ->
	Q = qlc:q([ [C#user_file.id, C#user_file.mime] || C <- mnesia:table(user_file),
      C#user_file.user_id == Uid, C#user_file.type == Type ]),
	dbd:do(Q).

delete_avatar_attr(_, [], _) -> ok;
delete_avatar_attr(Uid, [{[{name,<<"avatar">>},{value, FileId}]}], FileId) ->
   db_msg:sys_notify(Uid, [<<"avatar/change">>, FileId]),
   dbd:delete(user_attr, <<"avatar">>);
delete_avatar_attr(_, [_], _) -> ok.

map_user_file([#user_file{id=Id,type=Type,mime=Mime}]) -> [Id, Type, Mime];
map_user_file([]) -> [].

get_avatar_real([]) -> [];
get_avatar_real([#user_attr{value=FileId}]) -> map_user_file(dbd:get(user_file, FileId)).

get_avatar(Uid) -> get_avatar_real(attr_get(Uid, <<"avatar">>)).

set_avatar(Uid, FileId) ->
   db_msg:sys_notify(Uid, [<<"avatar/change">>, FileId]),
   attr_set(Uid, <<"avatar">>, FileId).

file_delete(Uid, FileId) ->
   Path = filename:join("store", integer_to_list(FileId)),
   file:delete(Path),
   delete_avatar_attr(Uid, attr_get(Uid, <<"avatar">>), FileId), 
   dbd:delete(user_file, FileId).

list_online(Limit) ->
	Q = qlc:q([ C#user_online.user_id || C <- mnesia:table(user_online) ]),
	detail(dbd:limit(Q, Limit)).

list_online(Offset, Limit) ->
   Q = qlc:q([ C#user_online.user_id || C <- mnesia:table(user_online) ]),
   detail(dbd:limit(Q, Offset, Limit)).

pids([H | T]) -> [ pids(H) ] ++ [ pids(Uid) || Uid <- T ];
pids(Uid) ->
	Q = qlc:q([ U#user_online.pid || U <- mnesia:table(user_online), U#user_online.user_id == Uid ]),
	dbd:do(Q).

sid_to_uid(Sid) ->
   case dbd:index(user_online, session_id, Sid) of
      [UO] -> UO#user_online.user_id;
      _    -> fail
   end.


select_one([]) -> [];
select_one([U|_]) -> [U].

% term could be user_id or username binary
lookup(Term) -> 
   try 
      Uid = erlang:binary_to_integer(Term),
      dbd:get(user, Uid)
   catch
      _:_ -> select_one(dbd:index(user, username, Term))
   end.

get_by_fb(Id) when is_binary(Id) -> dbd:index(user, facebook_id, Id);
get_by_fb(_) -> [].

get_by_email(Id) when is_binary(Id) -> dbd:index(user, email, Id);
get_by_email(_) -> [].

get_by_name(Name) -> dbd:index(user, username, Name).

clear_online([]) -> ok;
clear_online([#user_online{id=Id} | T]) -> dbd:delete(user_online, Id), clear_online(T).

drop_online_status([]) -> ok;
drop_online_status([UO = #user_online{id=Id, pid=Pid, user_id=Uid, session_id=Sid} | R]) ->
   ?INFO("offline: uo_id=~p user_id=~p sid=~p pid=~p", [Id, Uid, Sid, Pid]),
   notify_conv(Uid, conv(Uid), [<<"offline">>, detail_short(Uid)]),
   dbd:put(UO#user_online{online=false, stamp=now()}),
   drop_online_status(R).

notify_logout([]) -> ok;
notify_logout([#user_online{id=Id, pid=Pid, user_id=Uid, session_id=Sid} | R]) ->
   ?INFO("logout: uo_id=~p user_id=~p sid=~p pid=~p", [Id, Uid, Sid, Pid]),
   notify_conv(Uid, conv(Uid), [<<"logout">>, detail_short(Uid)]),
   dbd:delete(user_online, Id),
   notify_logout(R).

offline(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status(R).

logout(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   notify_logout(R).

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
      session_id=Sid,
      online=true}),
   notify_conv(Uid, conv(Uid), [<<"online">>, db_user:detail_short(Uid)]),
   Sid;

handle_online_status([UO], Uid, Pid) ->
   ?INFO("user_online: already online, skip: user_id=~p pid=~p", [Uid, Pid]),
   dbd:put(UO#user_online{pid=Pid, online=true, stamp=now()}),
   UO#user_online.session_id.

online(Uid) -> handle_online_status(get_online_status(Uid),Uid,self()).

% friends

map_status_result([]) -> offline;
map_status_result(R) when is_list(R) -> online.

add_online_status({UserProps}) ->
   Uid = proplists:get_value(id, UserProps),
   Status = get_online_status(Uid),
   {UserProps ++ [{status, map_status_result(Status)}]}.

get_friends_ids(Uid) ->
   Q = qlc:q([ C#user_friend.friend_id || C <- mnesia:table(user_friend),
      C#user_friend.user_id == Uid ]),
   dbd:do(Q).

get_friends(Uid) -> [ add_online_status(detail_short(FriendId)) || FriendId <- get_friends_ids(Uid) ].

del_friend_record(#user_friend{id=Id}) -> dbd:delete(user_friend, Id).

del_friend(Uid, FriendId) -> del_friend(check_friendship(Uid, FriendId)).
del_friend(List) when is_list(List) -> lists:foreach(fun del_friend_record/1, List).

add_friend(Uid, FriendId) -> add_friend(Uid, FriendId, check_friendship(Uid, FriendId)).

add_friend(_Uid, _FriendId, [_F|_Rest]) -> ok;
add_friend(Uid, FriendId, []) ->
   dbd:put(#user_friend{
      id=dbd:next_id(user_friend),
      stamp=now(),
      user_id=Uid,
      friend_id=FriendId,
      type=friend
   }).

check_friendship(Uid, FriendId) ->
   Q = qlc:q([ C || C <- mnesia:table(user_friend),
      C#user_friend.user_id == Uid, C#user_friend.friend_id == FriendId ]),
   dbd:do(Q).