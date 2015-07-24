-module(logger).
-compile(export_all).
-include_lib("cmon/include/config.hrl").

get_app() ->
   case application:get_application() of
      {ok, App} -> atom_to_list(App);
      _         -> "_"
   end.

check_log(true, Module, Line, _String, Args, Fun) -> log(Module, Line, _String, Args, Fun);
check_log(_, _, _, _, _, _) -> skip.

member_of(_Module, []) -> true;
member_of(Module, List) -> lists:member(Module, List).

check(true, false) -> true;
check(_, _) -> false.

log_f(Module, Line, _String, Args, Fun) ->
   Check = check(
      member_of(Module, ?CFG(log_modules, [])),
      member_of(Module, ?CFG(skip_modules))
   ),
   check_log(Check, Module, Line, _String, Args, Fun).

log(Module, Line, _String, Args, Fun) ->
   String = get_app() ++ "/" ++ atom_to_list(Module) ++ "." ++ integer_to_list(Line) ++ ": " ++ _String ++ "~n",
   error_logger:Fun(String, Args).

info(Module, Line, String, Args)       -> log_f(Module, Line, String, Args, info_msg).
warning(Module, Line, String, Args)    -> log_f(Module, Line, String, Args, warning_msg).
error(Module, Line, String, Args)      -> log_f(Module, Line, String, Args, error_msg).
