-module(user_file).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

store([[Id, _]], Mime, Store, _, _, Data) ->
   [File] = dbd:get(user_file, Id),
   Path = filename:join(Store, integer_to_list(Id)),
   file:write_file(Path, Data),
   dbd:put(File#user_file{ mime=Mime }),
   Id;

store([], Mime, Store, Uid, Type, Data) ->
   FileId = dbd:make_uid(),
   Path = filename:join(Store, integer_to_list(FileId)),
   file:write_file(Path, Data),
   dbd:put(#user_file{ id=FileId, user_id=Uid, stamp=now(), mime=Mime, type=Type }),
   FileId.

store(Store, Data, Uid, Type, Mime) when is_number(Uid) ->
   store( db_user:files(Uid, Type), Mime, Store, Uid, Type, Data).
