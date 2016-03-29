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

-include("../src/occi_rendering.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ctx, #parse_ctx{ url = uri:from_string("http://localhost:8080/collections") } ).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(occi),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(occi),
    ok = application:stop(mnesia),
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
    [load_extension,
     load_import].


load_extension(Config) -> 
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    {ok, Bin} = file:read_file(ExtFile),
    Ext = occi_extension:load(xml, Bin, ?ctx),
    occi_models:import(Ext),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"network">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storage">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storagelink">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/network#">>, <<"ipnetwork">>}))),
    ?assertMatch(mixin, 
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/networkinterface#">>, <<"ipnetworkinterface">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"os_tpl">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"resource_tpl">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"large">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://occi.example.org/occi/infrastructure/os_tpl#">>, <<"debian6">>}))),
    A0 = {<<"http://schemas.ogf.org/occi/infrastructure/compute/action#">>, <<"start">>},
    ?assertMatch(action, occi_category:class(occi_models:category(A0))),
    A1 = {<<"http://schemas.ogf.org/occi/infrastructure/compute/action#">>, <<"stop">>},
    ?assertMatch(action, occi_category:class(occi_models:category(A1))),
    A2 = {<<"http://schemas.ogf.org/occi/infrastructure/compute/action#">>, <<"restart">>},
    ?assertMatch(action, occi_category:class(occi_models:category(A2))),
    A3 = {<<"http://schemas.ogf.org/occi/infrastructure/compute/action#">>, <<"suspend">>},
    ?assertMatch(action, occi_category:class(occi_models:category(A3))),
    ok.


load_import(_Config) ->
    Ext = occi_extension:load(xml, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				     "<occi:extension xmlns:occi=\"http://schemas.ogf.org/occi\""
				     " name=\"Custom\""
				     " scheme=\"http://example.org/occi#\""
				     " status=\"stable\" version=\"1\">"
				     " <occi:import scheme=\"http://schemas.ogf.org/occi/infrastructure#\" />"
				     "</occi:extension>">>, ?ctx),
    occi_models:import(Ext),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"network">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storage">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storagelink">>}))),
    ?assertMatch(kind,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/network#">>, <<"ipnetwork">>}))),
    ?assertMatch(mixin, 
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/networkinterface#">>, <<"ipnetworkinterface">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"os_tpl">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"resource_tpl">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"large">>}))),
    ?assertMatch(mixin,
		 occi_category:class(occi_models:category({<<"http://occi.example.org/occi/infrastructure/os_tpl#">>, <<"debian6">>}))),
    ok.
