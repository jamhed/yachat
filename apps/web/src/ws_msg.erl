-module(ws_msg).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

send_msg(ConvId, UserId, Message) ->
   MsgId = db_msg:put(ConvId, UserId, Message),
   db_conv:notify(ConvId, UserId, MsgId),
   MsgId.

%msg p2p message, create conv if not exists
msg(M = <<"msg/p2p">>, [UserId, PeerId, Message]) when is_number(UserId), is_number(PeerId) ->
   case db_conv:find(UserId, PeerId, "p2p") of
      {ok, #conv{id=Cid}} ->
         MsgId = send_msg(Cid, UserId, Message),
         [M, ok, Cid, MsgId];
      _       ->
         Cid = db_conv:p2p(UserId, PeerId),
         MsgId = send_msg(Cid, UserId, Message),
         [M, ok, Cid, MsgId]
   end;

%msg conv message
msg(M = <<"msg/conv">>, [UserId, ConvId, Message]) when is_number(UserId), is_number(ConvId)  ->
   MsgId = send_msg(ConvId, UserId, Message),
   [M, ok, ConvId, MsgId];

msg(_,_) -> skip.
