-module(ws_user_attr).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-import(db_attr, [attr_map/1,get/2,set/3,del/2,list/1]).

%msg get user's attribute
msg(M = <<"user/attr/get">>, [Uid, Name]) when is_number(Uid) ->
   [M] ++ db_util:jiffy_wrapper(attr_map(get(Uid, Name)));

msg(M = <<"user/attr/del">>, [Uid, Name]) when is_number(Uid) ->
   [M] ++ [del(Uid, Name)];

%msg list user's attributes
msg(M = <<"user/attr/list">>, [Uid]) when is_number(Uid) ->
   [M] ++ [db_util:jiffy_wrapper(attr_map(list(Uid)))];

%msg set user's attribute
msg(M = <<"user/attr/set">>, [Uid, Name, Value]) when is_number(Uid) ->
   [M] ++ set(Uid, Name, Value);

%msg set user's attributes from json object [{k,v}, ..., {k,v}]
msg(M = <<"user/attr/set">>, [Uid, {Plist}]) when is_number(Uid) ->
   [M] ++ lists:flatten([ db_attr:set(Uid, P, proplists:get_value(P,Plist)) || P <- proplists:get_keys(Plist) ]);

msg(_, _) -> skip.