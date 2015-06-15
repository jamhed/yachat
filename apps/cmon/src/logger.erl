-module(logger).
-compile(export_all).

-include_lib("cmon/include/config.hrl").

get_app() ->
   case application:get_application() of
      {ok, App} -> atom_to_list(App);
      _         -> "_"
   end.


log_f(Module, Line, _String, Args, Fun) ->
   case List = ?CFG(log_modules, []) of
      [] -> log(Module, Line, _String, Args, Fun);
      _  ->
         case lists:member(Module, List) of
            true -> log(Module, Line, _String, Args, Fun);
            false -> skip
         end
   end.

log(Module, Line, _String, Args, Fun) ->
   String = get_app() ++ "/" ++ atom_to_list(Module) ++ "." ++ integer_to_list(Line) ++ ": " ++ _String ++ "~n",
   error_logger:Fun(String, Args).


info(Module, Line, String, Args)       -> log_f(Module, Line, String, Args, info_msg).
warning(Module, Line, String, Args)    -> log_f(Module, Line, String, Args, warning_msg).
error(Module, Line, String, Args)      -> log_f(Module, Line, String, Args, error_msg).
