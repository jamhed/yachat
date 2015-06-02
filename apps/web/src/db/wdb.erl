-module(wdb).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

metainfo() -> 
    #schema{name=dbd,tables=[
        #table{name=user,fields=record_info(fields,user), keys=[facebook_id, email]},
        #table{name=conv,fields=record_info(fields,conv), keys=[]},
        #table{name=user_conv, fields=record_info(fields, user_conv), keys=[user_id, conv_id]},
        #table{name=conv_msg, fields=record_info(fields, conv_msg), keys=[]},
        #table{name=user_msg, fields=record_info(fields, user_msg), keys=[]},
        #table{name=message, fields=record_info(fields, message), keys=[user_id]},
        #table{name=user_online, fields=record_info(fields, user_online), keys=[user_id, pid]}
    ]}.




