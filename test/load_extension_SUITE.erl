%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(load_extension_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


init_per_group(_GroupName, Config) ->
    Config.


end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    [].


all() -> 
    [load_extension].


load_extension(Config) -> 
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    occi_models:load_path(xml, ExtFile),
    ?assertMatch(kind, 
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "compute"}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "network"}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "storage"}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "storagelink"}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "networkinterface"}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure/network#", "ipnetwork"}))),
    ?assertMatch(mixin, 
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure/networkinterface#", "ipnetworkinterface"}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "os_tpl"}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "resource_tpl"}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({"http://schemas.ogf.org/occi/infrastructure#", "large"}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({"http://occi.example.org/occi/infrastructure/os_tpl#", "debian6"}))),
    ok.
