-module(ws_game).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

% tile = [Color = 1..4, Number = 1..13]

msg(M = <<"game/start">>, [Uid]) -> [M, ok];

msg(M = <<"game/stop">>, [Uid]) -> [M, ok];

msg(M = <<"game/take">>, [Uid, <<"left">>]) -> [M, ok];

msg(M = <<"game/take">>, [Uid, <<"center">>]) -> [M, ok];

msg(M = <<"game/discard">>, [Uid, [C,N]]) -> [M, ok];

msg(_,_) -> skip.