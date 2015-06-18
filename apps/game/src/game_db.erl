-module(game_db).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("game/include/game.hrl").

metainfo() -> 
    #schema{name=game_db,tables=[
        #table{name=player,fields=record_info(fields,player), keys=[]},
        #table{name=game,fields=record_info(fields,game), keys=[]}
    ]}.
