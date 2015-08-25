-module(db_session).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-compile({no_auto_import,[get/1,get/2,set/3]}).
-compile(export_all).

set(Sid, Name, Value) ->
   dbd:put(#user_session_data{id={Sid,Name}, name=Name, value=Value, session_id=Sid}).

get(Sid, Name) ->
   [ Value || #user_session_data{value=Value} <- dbd:get(user_session_data, {Sid, Name}) ].

get(Sid) -> [ [{name, Name}, {value,Value}] || 
   #user_session_data{name=Name, value=Value} <- dbd:index(user_session_data, session_id, Sid) ].

del(Sid, Name) -> dbd:delete(user_session_data, {Sid,Name}).