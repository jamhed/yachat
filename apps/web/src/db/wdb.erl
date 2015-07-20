-module(wdb).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

metainfo() -> 
    #schema{name=dbd,tables=[
    	?TABLE(user, [facebook_id, email, username]),
    	?TABLE(conv, []),
    	?TABLE(message, [user_id]),
    	?TABLE(user_attr, [user_id]),
    	?TABLE(user_conv, [user_id, conv_id]),
    	?TABLE(user_online, [user_id, pid, session_id]),
    	?TABLE(user_file, [user_id]),
    	?TABLE(user_friend, [user_id, friend_id])
    ]}.
