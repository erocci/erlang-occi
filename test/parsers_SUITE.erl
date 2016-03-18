%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 8 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(parsers_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(occi),
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    {ok, Xml} = file:read_file(ExtFile),
    ok = occi_models:import(occi_parser_xml:parse_model(extension, Xml)),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(occi),
    ok = application:stop(mnesia),
    ok.


init_per_group(core_resource, Config) ->
    Fun = fun(R)  ->
		  ?assertMatch("http://example.org:8080/resource1", occi_resource:id(R)),
		  ?assertMatch({"http://schemas.ogf.org/occi/core#", "resource"}, occi_resource:kind(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group(core_link, Config) ->
    Fun = fun(L)  ->
		  ?assertMatch("http://example.org:8080/link1", occi_link:id(L)),
		  ?assertMatch({"http://schemas.ogf.org/occi/core#", "link"}, occi_link:kind(L)),
		  ?assertMatch("/myresource0", occi_link:source(L)),
		  ?assertMatch("/myresource1", occi_link:target(L)),
		  ?assertMatch(#{}, occi_link:attributes(L))
	  end,
    [ {check, Fun} | Config ];

init_per_group('netif_link', Config) ->
    Fun = fun(L)  ->
		  ?assertMatch("http://example.org:8080/netif1", occi_link:id(L)),
		  ?assertMatch({"http://schemas.ogf.org/occi/infrastructure#", "networkinterface"}, occi_link:kind(L)),
		  ?assertMatch([{"http://schemas.ogf.org/occi/infrastructure/networkinterface#", "ipnetworkinterface"}], 
			       occi_link:mixins(L)),
		  ?assertMatch(#{ "occi.core.source" := "http://example.org:8080/compute1",
				  "occi.core.target" := "http://example.org:8080/network1",
				  "occi.networkinterface.interface" := "eth0",
				  "occi.networkinterface.mac" := "00:11:22:33:44:55",
				  "occi.networkinterface.address" := "192.168.0.1",
				  "occi.networkinterface.allocation" := static }, 
			       occi_link:attributes(L))
	  end,
    [ {check, Fun} | Config ];

init_per_group('resource_link', Config) ->
    Fun = fun(R)  ->
		  ?assertMatch("http://example.org:8080/resource1", occi_resource:id(R)),
		  ?assertMatch({"http://schemas.ogf.org/occi/core#", "resource"}, occi_resource:kind(R)),
		  [Link] = occi_resource:links(R),
 		  ?assertMatch("mylink1", occi_link:id(Link)),
 		  ?assertMatch({"http://schemas.ogf.org/occi/core#", "link"}, occi_link:kind(Link)),
 		  ?assertMatch("http://example.org/another_resource1", occi_link:target(Link)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group('compute_a', Config) ->
    Fun = fun(R)  ->
		  ?assertMatch("http://example.org:8080/compute1", occi_resource:id(R)),
		  ?assertMatch({"http://schemas.ogf.org/occi/infrastructure#", "compute"}, occi_resource:kind(R)),
		  ?assertMatch(#{ "occi.core.summary" := "A super computer",
				  "occi.compute.cores" := 45,
				  "occi.compute.hostname" := "a name",
				  "occi.compute.speed" := 1.5,
				  "occi.compute.state" := inactive }, 
			       occi_resource:attributes(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group(_, Config) ->
    Config.


end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    [
     {core_resource,       [], [parse_xml, parse_text, parse_json]}
    ,{core_link,           [], [parse_xml, parse_text, parse_json]}
    ,{'resource_link',     [], [parse_xml, parse_text, parse_json]}
    ,{'compute_a',         [], [parse_xml, parse_text, parse_json]}
    ,{'netif_link',        [], [parse_xml]}
    ].


all() -> 
    [
     {group, core_resource}
    ,{group, core_link}
    ,{group, 'resource_link'}
    ,{group, 'compute_a'}
    ,{group, 'netif_link'}
    ].


%%%
%%% Tests
%%%
parse_text(Config) -> parse_tests(text, ".occi", Config).

parse_xml(Config) -> parse_tests(xml, ".xml", Config).

parse_json(Config) -> parse_tests(json, ".json", Config).


%%%
%%% Internal
%%%
parse_tests(Type, Ext, Config) ->
    Basename = atom_to_list(proplists:get_value(name, ?config(tc_group_properties, Config))),
    lists:foreach(fun (Filename) ->
			  parse_test(Type, Filename, Config)
		  end, filelib:wildcard(filename:join([?config(data_dir, Config), Basename]) ++ "*" ++ Ext)).


parse_test(Type, Filename, Config) ->
    ct:pal(info, ?STD_IMPORTANCE, "=== Parsing ~s", [filename:basename(Filename)]),
    {ok, Bin} = file:read_file(Filename),
    Entity = occi_entity:load(Type, Bin, client),
    Fun = ?config(check, Config),
    Fun(Entity).
