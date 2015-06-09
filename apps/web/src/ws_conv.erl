-module(ws_conv).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

% list users in conversation
conv_users(Uid, ConvId) when is_number(Uid) ->
   List = db_conv:users(ConvId),
   UserDetailList = db_user:detail(List),
   [ok, UserDetailList];
conv_users(_,_) -> [fail, args].

% message history
conv_history(Uid, ConvId) when is_number(Uid), is_number(ConvId) ->
   History = [ [db_user:detail(Uid), [cvt:now_to_time_binary(Stamp), Text]]
      || #message{stamp=Stamp,text=Text,user_id=Uid} <- db_conv:history(ConvId) ],
   [ok, History];
conv_history(_,_) -> [fail, agrs].

% new conversation
conv_new(Uid) when is_number(Uid) ->
   ConvId = db_conv:generic(Uid),
   [ok, ConvId];
conv_new(_) -> [fail, args].

% join conversation
conv_join(Uid, ConvId) when is_number(Uid) -> db_conv:join(Uid, ConvId), [ok];
conv_join(_,_) -> [fail, args].

% leave conversation
conv_leave(Uid, ConvId) when is_number(Uid) -> db_conv:leave(Uid, ConvId), [ok];
conv_leave(_,_) -> [fail, args].

%msg group list
msg(M = <<"conv/users">>, [Sid, ConvId]) when is_number(Sid), is_number(ConvId) ->
   [M] ++ conv_users(db_user:sid_to_pid(Sid), ConvId);

%msg message history
msg(M = <<"conv/history">>, [Sid, ConvId]) when is_number(Sid), is_number(ConvId)  ->
   [M] ++ conv_history(db_user:sid_to_pid(Sid), ConvId);

%msg create group
msg(M = <<"conv/new">>, [Sid]) when is_number(Sid) ->
   [M] ++ conv_new(db_user:sid_to_pid(Sid));

%msg join group
msg(M = <<"conv/join">>, [Sid, ConvId]) when is_number(Sid), is_number(ConvId) ->
   [M] ++ conv_join(db_user:sid_to_pid(Sid), ConvId);

%msg leave group
msg(M = <<"conv/leave">>, [Sid, ConvId]) when is_number(Sid), is_number(ConvId)  ->
   [M] ++ conv_leave(db_user:sid_to_pid(Sid), ConvId);

msg(_,_) -> skip.
