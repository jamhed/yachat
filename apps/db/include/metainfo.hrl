-ifndef(METAINFO_HRL).
-define(METAINFO_HRL, true).

-record(schema,{name,tables=[]}).
-record(table,{name,fields=[],keys=[],copy_type=disc_copies}).
-record(id_seq, {name, id}).

-endif.
