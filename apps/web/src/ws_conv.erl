-module(ws_conv).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg group list
msg(M = <<"conv/users">>, [UserId, ConvId]) when is_number(UserId), is_number(ConvId) ->
   List = db_conv:users(ConvId),
   UserDetailList = db_user:detail(List),
   [M, ok, UserDetailList];

%msg message history
msg(M = <<"conv/history">>, [ConvId]) when is_number(ConvId) ->
   History = [ [db_user:detail(Uid), [cvt:now_to_time_binary(Stamp), Text]]
      || #message{stamp=Stamp,text=Text,user_id=Uid} <- db_conv:history(ConvId) ],
   [M, ok, History];

%msg create group
msg(M = <<"conv/new">>, [UserId]) when is_number(UserId) ->
   ConvId = db_conv:generic(UserId),
   [M, ok, ConvId];

%msg join group
msg(M = <<"conv/join">>, [UserId, ConvId]) when is_number(UserId), is_number(ConvId) ->
   db_conv:join(UserId, ConvId),
   [M, ok, ConvId];

%msg leave group
msg(M = <<"conv/leave">>, [UserId, ConvId]) when is_number(UserId), is_number(ConvId)  ->
   db_conv:leave(UserId,ConvId),
   [M, ok, UserId, ConvId];

msg(_,_) -> skip.
