-module(logger).
-compile(export_all).

log(Module, Line, _String, Args, Fun) ->
   case lists:member(Module, cfg:get(log_modules, [])) of
      true ->
         String = atom_to_list(Module) ++ "." ++ integer_to_list(Line) ++ ": " ++ _String ++ "~n",
         error_logger:Fun(String, Args);
      false ->
         skip
   end.

info(Module, Line, String, Args)       -> log(Module, Line, String, Args, info_msg).
warning(Module, Line, String, Args)    -> log(Module, Line, String, Args, warning_msg).
error(Module, Line, String, Args)      -> log(Module, Line, String, Args, error_msg).
