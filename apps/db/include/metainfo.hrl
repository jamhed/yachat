-ifndef(METAINFO_HRL).
-define(METAINFO_HRL, true).

-record(schema, {name, tables=[]}).
-record(table, {name, fields=[], keys=[], copy_type=disc_copies}).
-record(id_seq, {name, id}).

% macros
-define(TABLE(Name, Keys), #table{name=Name, fields=record_info(fields, Name), keys=Keys}).

-endif.
