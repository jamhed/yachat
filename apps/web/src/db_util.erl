-module(db_util).
-compile(export_all).

jiffy_wrapper(List) when is_list(List) -> [ {Item} || Item <- List ];
jiffy_wrapper(Item) -> {Item}.

to_bool([_]) -> true;
to_bool([]) -> false.