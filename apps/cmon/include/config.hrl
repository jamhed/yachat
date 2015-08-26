-define(CFG(Key), cfg:get_m(?MODULE, Key)).
-define(CFG(Key, Default), cfg:get_m(?MODULE, Key, Default)).
-define(CFG_EXISTS(Name, Error), cfg:map_cfg(cfg:get_m(?MODULE, Name), Error)).
-define(CFG_EXISTS(Name), cfg:map_cfg(cfg:get_m(?MODULE, Name),
	?MODULE_STRING ++ ": no parameter '" ++ erlang:atom_to_list(Name) ++ "' in config.")).