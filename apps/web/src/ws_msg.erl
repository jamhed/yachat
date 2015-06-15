-module(ws_msg).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

send_msg([#conv{id=ConvId}], UserId, Message) when is_number(ConvId), is_number(UserId) ->
   MsgId = db_msg:put(ConvId, UserId, Message),
   [#message{text=Text,stamp=Stamp}] = db_msg:get(MsgId),
   db_conv:notify(ConvId, UserId, Stamp, Text),
   MsgId.

msg_p2p(Uid, PeerId, Message) when is_number(Uid), is_number(PeerId) ->
   [#conv{id=Cid}] = db_conv:find(Uid, PeerId, "p2p"),
   send_msg(Cid, Uid, Message);
   % Cid = db_conv:p2p(UserId, PeerId),
msg_p2p(_,_,_) -> [fail, args].

%msg p2p message, create conv if not exists
msg(M = <<"msg/p2p">>, [Sid, PeerId, Message]) when is_number(Sid), is_number(PeerId) ->
   [M] ++ msg_p2p(db_conv:sid_to_pid(Sid), PeerId, Message);

%msg conv message
msg(M = <<"msg/conv">>, [UserId, ConvId, Message]) when is_number(UserId), is_number(ConvId)  ->
   MsgId = send_msg(db_conv:get(ConvId), UserId, Message),
   [M, ok, ConvId, MsgId];

msg(_,_) -> skip.
