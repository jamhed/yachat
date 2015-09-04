-module(db_func).
-compile(export_all).

nv_to_tuple({[{<<"name">>,Name},{<<"value">>,Value}]}) -> {erlang:binary_to_atom(Name, utf8), Value};
nv_to_tuple({[{<<"name">>,Name}]}) -> {erlang:binary_to_atom(Name, utf8), null}.


form_to_plist(List) when is_list(List) -> [nv_to_tuple(P) || P <- List];
form_to_plist(E) -> form_to_plist([E]).

to_list(U) -> [ map_field(F) || F <- tl(tuple_to_list(U)) ].

% field mapper
map_field(undefined) -> null;
map_field(Now = {_,_,_}) -> cvt:now_to_binary(Now);
map_field(F) -> F.