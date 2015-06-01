-module(cvt).
-compile(export_all).

now_to_str(Now) ->
   {{Y,M,D},{H,MM,S}} = calendar:now_to_local_time(Now),
   io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",[Y,M,D,H,MM,S]).

now_to_time_str(Now) ->
   {_,{H,MM,S}} = calendar:now_to_local_time(Now),
   io_lib:format("~2..0B:~2..0B:~2..0B",[H,MM,S]).

now_to_date_str(Now) ->
   {{Y,M,D}, _} = calendar:now_to_local_time(Now),
   io_lib:format("~4..0B-~2..0B-~2..0B",[Y,M,D]).

now_to_binary(Now) -> erlang:list_to_binary(now_to_str(Now)).
now_to_time_binary(Now) -> erlang:list_to_binary(now_to_time_str(Now)).
now_to_date_binary(Now) -> erlang:list_to_binary(now_to_date_str(Now)).

date_to_str({Y,M,D}) -> io_lib:format("~4..0B-~2..0B-~2..0B", [Y,M,D]).


