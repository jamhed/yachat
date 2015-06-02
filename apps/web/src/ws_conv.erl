-module(ws_conv).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

% group list
msg(M = <<"conv/list">>, [UserId, ConvId]) ->
   List = wdb:list(UserId, ConvId),
   Full = wdb:users_detail(List),
   [M, ok, Full];

msg(M = <<"conv/history">>, [Cid]) ->
   H = [ [db_user:map_uid(Uid), [cvt:now_to_time_binary(Stamp), Text]]
      || #message{stamp=Stamp,text=Text,user_id=Uid} <- wdb:conv_history(Cid) ],
   [M, ok, H];

% create group
msg(M = <<"conv/new">>, [null]) ->
   [M, fail, null_id];

msg(M = <<"conv/new">>, [UserId]) ->
   Cid = wdb:make_conv(UserId),
   [M, ok, Cid];

% join group
msg(M = <<"conv/join">>, [UserId, ConvId]) ->
   wdb:join_conv(UserId, ConvId),
   [M, ok, ConvId];

% leave group
msg(M = <<"conv/leave">>, [UserId, ConvId]) ->
   wdb:leave_conv(UserId,ConvId),
   [M, ok, UserId, ConvId];

msg(_,_) -> skip.
