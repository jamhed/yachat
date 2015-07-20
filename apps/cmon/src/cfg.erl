-module(cfg).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").

-define(PATH, "cfg").
-define(ETS_NAME, cfg).

validate(Props) ->
   case proplists:get_value(fail, Props) of
      undefined -> ok;
      _ -> erlang:error({config_error, Props})
   end.

ensure_ets_table() ->
   case ets:info(?ETS_NAME) of
      undefined -> ets:new(?ETS_NAME, [set,named_table,public]);
      _  -> ?ETS_NAME
   end.

handle_read_file(Module, {error,enoent}) -> erlang:error({no_config_file_for_module, Module});
handle_read_file(Module, {ok,[Cfg]}) -> Cfg;
handle_read_file(Module, Err) -> erlang:error({config_file_read_error, Err}).

handle_module_cfg(Module, []) ->
   Path = filename:join("cfg", Module),
   Cfg = handle_read_file(Module, file:consult(Path)),
   ets:insert(?ETS_NAME, {Module, Cfg}),
   ?INFO("Loaded config for module:~p, path:~p, data:~180p", [Module, Path, Cfg]),
   Cfg;
handle_module_cfg(Module, [{Module, PropList}]) -> PropList.

ensure_module_cfg(Module) ->
   handle_module_cfg( Module, ets:lookup(?ETS_NAME, Module) ).
   
get_m(Module, Key) ->
   ensure_ets_table(),
   Cfg = ensure_module_cfg(Module),
   Value = proplists:get_value(Key, Cfg),
   Value.

get_m(Module, Key, Default) ->
   ensure_ets_table(),
   Cfg = ensure_module_cfg(Module),
   Value = proplists:get_value(Key, Cfg, Default),
   Value.