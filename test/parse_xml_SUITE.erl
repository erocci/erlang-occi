%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(parse_xml_SUITE).

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


init_per_group(infrastructure, Config) ->
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    {ok, Xml} = file:read_file(ExtFile),
    ok = occi_models:import(occi_parser_xml:parse_model(extension, Xml)),
    Config;

init_per_group(resources, Config) ->
    Config;

init_per_group(_GroupName, Config) ->
    Config.


end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    [
     {extension, [], [ parse_extension ]},
     {resources, [], 
      [ 
	parse_resource1
      ,parse_resource2_bad
      ,parse_resource_link1
      ]},
     {infrastructure,
      [
       parse_compute1
      ]}
    ].


all() -> 
    [
     {group, extension},
     {group, resources},
     {group, infrastructure}
    ].


parse_extension(Config) -> 
    Xml = read_file(Config, "occi-infrastructure.xml"),
    Ext = occi_extension:load(xml, Xml),
    ct:log(info, "extension: ~p", [Ext]),
    ?assertMatch("Infrastructure", occi_extension:name(Ext)),
    ?assertMatch("http://schemas.ogf.org/occi/infrastructure#", occi_extension:scheme(Ext)),
    ?assertEqual(5, length(occi_extension:kinds(Ext))),
    ?assertEqual(6, length(occi_extension:mixins(Ext))),
    ok.

parse_resource1(Config) ->
    Bin = read_file(Config, "resource1.xml"),
    Res = occi_resource:load(xml, Bin, client),
    ?assertMatch({"http://schemas.ogf.org/occi/core#", "resource"}, occi_resource:kind(Res)).


parse_resource2_bad(Config) ->
    Bin = read_file(Config, "resource2.xml"),
    ?assertThrow({parse_error, _, {unknown_category, {"http://schemas.ogf.org/occi/core#", "unknown"}}}, 
		 occi_resource:load(xml, Bin, client)).


parse_resource_link1(Config) ->
    Bin = read_file(Config, "resource_link1.xml"),
    Res = occi_resource:load(xml, Bin, client),
    ?assertMatch({"http://schemas.ogf.org/occi/core#", "resource"}, occi_resource:kind(Res)).


parse_compute1(Config) ->
    Bin = read_file(Config, "compute1.xml"),
    Res = occi_resource:load(xml, Bin, client),
    ?assertMatch({"http://schemas.ogf.org/occi/infrastructure#", "compute"}, occi_resource:kind(Res)),
    ?assertMatch(45, occi_resource:get("occi.compute.cores", Res)),
    ?assertMatch("a name", occi_resource:get("occi.compute.hostname", Res)),
    ?assertMatch(1.5, occi_resource:get("occi.compute.speed", Res)).

%%%
%%% Internal
%%%
read_file(Config, Path) ->
    Fullpath = filename:join([?config(data_dir, Config), Path]),
    {ok, Bin} = file:read_file(Fullpath),
    Bin.
