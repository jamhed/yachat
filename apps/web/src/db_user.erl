-module(db_user).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

get(Id) -> dbd:get(user, Id).
get_by_fb(Id) -> dbd:index(user, facebook_id, Id).
get_by_email(Id) -> dbd:index(user, email, Id).

clear_online([]) -> ok;
clear_online([#user_online{id=Id} | T]) -> dbd:delete(user_online, Id), clear_online(T).

drop_online_status([]) -> ok;

drop_online_status([#user_online{id=Id, pid=Pid} | R]) ->
   ?INFO("drop_online_status: id=~p pid=~p", [Id, Pid]),
   dbd:delete(user_online, Id),
   drop_online_status(R).

offline(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status(R).

online(#user{id=Uid}, Pid) ->
   case dbd:index(user_online, pid, Pid) of
      [] ->
         ?INFO("user_online: user_id=~p pid=~p", [Uid, Pid]),
         dbd:put(#user_online{id=dbd:next_id(user_online,1), stamp=now(), pid=Pid, user_id=Uid});
      [_ | _] ->
         ?INFO("user_online: already online, skip: user_id=~p pid=~p", [Uid, Pid])
   end.

map_uid(Uid) ->
   case get(Uid) of
      {ok, #user{id=Uid,username=Name,email=Email}}  -> [Uid,Name,Email];
      _        -> []
   end.



