-module(db_func).
-compile(export_all).

nv_to_tuple({[{<<"name">>,Name},{<<"value">>,Value}]}) -> {erlang:binary_to_atom(Name, utf8), Value}.

form_to_plist(List) -> [nv_to_tuple(P) || P <- List].