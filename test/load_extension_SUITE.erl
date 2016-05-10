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
    {ok, _} = application:ensure_all_started(occi),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(occi),
    ok = application:stop(mnesia),
    ok.


init_per_group(load, Config) ->
    Config;

init_per_group(models, Config) ->
    Bin = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	    "<occi:extension xmlns:occi=\"http://schemas.ogf.org/occi\""
	    " name=\"Custom\""
	    " scheme=\"http://example.org/occi#\""
	    " status=\"stable\" version=\"1\">"
	    " <occi:import scheme=\"http://schemas.ogf.org/occi/infrastructure#\" />"
	    "</occi:extension>">>,
    Ext = occi_rendering:parse(xml, Bin, occi_extension),
    occi_models:import(Ext),
    Config.


end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    [
     {load,   [], [load_extension, load_import]}
    ,{models, [], [categories, locations]}
    ].


all() -> 
    [
     {group, load}
    ,{group, models}
    ].


load_extension(Config) -> 
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    Ext = occi_rendering:parse_file(ExtFile, occi_extension),
    occi_models:import(Ext),

    ComputeKind = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}),
    ?assertMatch(kind, occi_category:class(ComputeKind)),

    NetworkKind = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"network">>}),
    ?assertMatch(kind, occi_category:class(NetworkKind)),

    StorageKind = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storage">>}),
    ?assertMatch(kind, occi_category:class(StorageKind)),

    StorageLinkKind = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storagelink">>}),
    ?assertMatch(kind, occi_category:class(StorageLinkKind)),

    NetworkInterfaceKind = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>}),
    ?assertMatch(kind, occi_category:class(NetworkInterfaceKind)),

    IpNetworkMixin = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/network#">>, <<"ipnetwork">>}),
    ?assertMatch(mixin, occi_category:class(IpNetworkMixin)),

    IpNetworkInterfaceMixin = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/networkinterface#">>, <<"ipnetworkinterface">>}),
    ?assertMatch(mixin, occi_category:class(IpNetworkInterfaceMixin)),

    OsTplMixin = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"os_tpl">>}),
    ?assertMatch(mixin, occi_category:class(OsTplMixin)),

    ResourceTplMixin = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"resource_tpl">>}),
    ?assertMatch(mixin, occi_category:class(ResourceTplMixin)),

    LargeMixin = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"large">>}),
    ?assertMatch(mixin, occi_category:class(LargeMixin)),

    DebianMixin = occi_models:category({<<"http://occi.example.org/occi/infrastructure/os_tpl#">>, <<"debian6">>}),
    ?assertMatch(mixin, occi_category:class(DebianMixin)),

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
    Ext = occi_rendering:parse(xml, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				      "<occi:extension xmlns:occi=\"http://schemas.ogf.org/occi\""
				      " name=\"Custom\""
				      " scheme=\"http://example.org/occi#\""
				      " status=\"stable\" version=\"1\">"
				      " <occi:import scheme=\"http://schemas.ogf.org/occi/infrastructure#\" />"
				      "</occi:extension>">>, occi_extension),
    occi_models:import(Ext),
    Cat0 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}),
    ?assertMatch(kind, occi_category:class(Cat0)).


categories(_Config) ->
    Cat0 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}),
    ?assertMatch(kind, occi_category:class(Cat0)),
    ?assertMatch(<<"/categories/compute">>, occi_category:location(Cat0)),

    Cat1 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"network">>}),
    ?assertMatch(kind, occi_category:class(Cat1)),
    ?assertMatch(<<"/categories/network">>, occi_category:location(Cat1)),

    Cat2 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storage">>}),
    ?assertMatch(kind, occi_category:class(Cat2)),
    ?assertMatch(<<"/categories/storage">>, occi_category:location(Cat2)),

    Cat3 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"storagelink">>}),
    ?assertMatch(kind, occi_category:class(Cat3)),
    ?assertMatch(<<"/categories/storagelink">>, occi_category:location(Cat3)),

    Cat4 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>}),
    ?assertMatch(kind, occi_category:class(Cat4)),
    ?assertMatch(<<"/categories/networkinterface">>, occi_category:location(Cat4)),

    Cat5 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/network#">>, <<"ipnetwork">>}),
    ?assertMatch(mixin, occi_category:class(Cat5)),
    ?assertMatch(<<"/categories/ipnetwork">>, occi_category:location(Cat5)),

    Cat6 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure/networkinterface#">>, 
				 <<"ipnetworkinterface">>}),
    ?assertMatch(mixin, occi_category:class(Cat6)),
    ?assertMatch(<<"/categories/ipnetworkinterface">>, occi_category:location(Cat6)),

    Cat7 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"os_tpl">>}),
    ?assertMatch(mixin, occi_category:class(Cat7)),
    ?assertMatch(<<"/categories/os_tpl">>, occi_category:location(Cat7)),

    Cat8 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"resource_tpl">>}),
    ?assertMatch(mixin, occi_category:class(Cat8)),
    ?assertMatch(<<"/categories/resource_tpl">>, occi_category:location(Cat8)),

    Cat9 = occi_models:category({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"large">>}),
    ?assertMatch(mixin, occi_category:class(Cat9)),
    ?assertMatch(<<"/categories/large">>, occi_category:location(Cat9)),

    Cat10 = occi_models:category({<<"http://occi.example.org/occi/infrastructure/os_tpl#">>, <<"debian6">>}),
    ?assertMatch(mixin, occi_category:class(Cat10)),
    ?assertMatch(<<"/categories/debian6">>, occi_category:location(Cat10)),
    ok.


locations(_Config) ->
    ?assertMatch({<<"http://schemas.ogf.org/occi/core#">>, <<"resource">>},
		 occi_category:id(occi_models:location(<<"/categories/resource">>))),

    ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>},
		 occi_category:id(occi_models:location(<<"/categories/compute">>))),

    ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"network">>},
		 occi_category:id(occi_models:location(<<"/categories/network">>))),

    ok = occi_models:add_category(occi_mixin:new(<<"http://schemas.example.org#">>, <<"compute">>)),
    ?assertMatch({<<"http://schemas.example.org#">>, <<"compute">>},
		 occi_category:id(occi_models:location(<<"/categories/compute0">>))),    
    ok.

