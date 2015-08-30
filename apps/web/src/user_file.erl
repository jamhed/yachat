-module(user_file).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-include_lib("cmon/include/config.hrl").

check_path_exists(Path) ->
   ok = filelib:ensure_dir(filename:join(Path, ".exists")).

check_config() ->
   cfg:validate([?CFG_EXISTS(path)]),
   check_path_exists(path()).
path() -> ?CFG(path). 

make_full_path(FileId) when is_integer(FileId) ->
   filename:join(path(), integer_to_list(FileId)).

delete(FileId) -> file:delete(make_full_path(FileId)).

% create
store(Data, Uid, Type, Mime) ->
   FileId = dbd:make_uid(),
   file:write_file(make_full_path(FileId), Data),
   dbd:put(#user_file{ id=FileId, user_id=Uid, stamp=now(), mime=Mime, type=Type }),
   FileId.