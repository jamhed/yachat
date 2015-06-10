-module(db_user).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-define(SYSTEM,1).

to_proplist(#user{} = U) -> lists:zip(record_info(fields, user), tl(tuple_to_list(U))).
to_list(U) -> tl(tuple_to_list(U)).

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
   
map_field(undefined) -> null;
map_field(Now = {_,_,_}) -> cvt:now_to_binary(Now);
map_field(F) -> F.

user_to_short_list({ok, #user{id=Id,username=Name,email=Email}}) -> [ map_field(F) || F <- [Id, Name, Email] ];
user_to_short_list(_) -> [].

user_to_list({ok, #user{
   id=Id,
   username=Name,
   email=Email,
   firstname=FirstName,
   lastname=LastName,
   gender=Gender,
   avatar=Avatar,
   birthdate=BirthDate,
   city=City} }) -> [ map_field(F) || F <- [Id,Name,Email,FirstName,LastName,Gender,Avatar,BirthDate,City] ];
user_to_list(_) -> [].

detail([H | T]) -> [detail(H)] ++ [ detail(U) || U <- T];
detail(Uid) -> user_to_list( dbd:get(user, Uid) ).

detail_short([H|T]) -> [detail_short(H)] ++ [detail_short(U) || U <- T];
detail_short(Uid) -> user_to_short_list( dbd:get(user, Uid) ).

% id of convs user is in
conv(Uid) ->
	Q = qlc:q([ C#user_conv.conv_id || C <- mnesia:table(user_conv), C#user_conv.user_id == Uid ]),
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

% compat to index
get(Id) when is_number(Id) -> get(dbd:get(user, Id));
get({ok,U}) -> [U];
get(_) -> [].

get_by_fb(Id) when is_binary(Id) -> dbd:index(user, facebook_id, Id);
get_by_fb(_) -> [].
get_by_email(Id) when is_binary(Id) -> dbd:index(user, email, Id);
get_by_email(_) -> [].

clear_online([]) -> ok;
clear_online([#user_online{id=Id} | T]) -> dbd:delete(user_online, Id), clear_online(T).

drop_online_status([]) -> ok;
drop_online_status([#user_online{id=Id, pid=Pid, user_id=Uid} | R]) ->
   ?INFO("drop_online_status: id=~p pid=~p", [Id, Pid]),
   notify_conv(<<"offline">>, conv(Uid)),
   dbd:delete(user_online, Id),
   drop_online_status(R).

drop_online_status_k([]) -> ok;
drop_online_status_k([#user_online{id=Id, pid=Pid, user_id=Uid} | R]) ->
   ?INFO("drop_online_status: id=~p pid=~p", [Id, Pid]),
   notify_conv(<<"offline">>, conv(Uid)),
   drop_online_status(R).


offline(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status_k(R).

logout(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status(R).

notify_conv(_,[]) -> ok;
notify_conv(Text, [H | T]) ->
   db_conv:sys_notify(H, Text),
   notify_conv(Text, T).

online(#user{id=Uid}, Pid) ->
   case dbd:index(user_online, pid, Pid) of
      [] ->
         ?INFO("user_online: user_id=~p pid=~p", [Uid, Pid]),
         Sid = dbd:make_uid(),
         dbd:put(#user_online{
            id=dbd:next_id(user_online),
            stamp=now(),
            pid=Pid,
            user_id=Uid,
            session_id=Sid}),
         notify_conv(<<"online">>, conv(Uid)),
         Sid;
      [UO] ->
         ?INFO("user_online: already online, skip: user_id=~p pid=~p", [Uid, Pid]),
         dbd:put(UO#user_online{pid=Pid}),
         UO#user_online.session_id
   end.
