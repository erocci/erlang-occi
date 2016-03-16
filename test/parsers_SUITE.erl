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
    Basename = filename:join([?config(data_dir, Config), "core_resource"]),
    Fun = fun(R)  ->
		  ?assertMatch("http://example.org:8080/resource1", occi_resource:id(R)),
		  ?assertMatch({"http://schemas.ogf.org/occi/core#", "resource"}, occi_resource:kind(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {basename, Basename}, {check, Fun} | Config ];

init_per_group(core_link, Config) ->
    Basename = filename:join([?config(data_dir, Config), "core_link"]),
    Fun = fun(L)  ->
		  ?assertMatch("http://example.org:8080/link1", occi_link:id(L)),
		  ?assertMatch({"http://schemas.ogf.org/occi/core#", "link"}, occi_link:kind(L)),
		  ?assertMatch("/myresource0", occi_link:source(L)),
		  ?assertMatch("/myresource1", occi_link:target(L)),
		  ?assertMatch(#{}, occi_link:attributes(L))
	  end,
    [ {basename, Basename}, {check, Fun} | Config ];

init_per_group('compute_a', Config) ->
    Basename = filename:join([?config(data_dir, Config), "compute_a"]),
    Fun = fun(R)  ->
		  ?assertMatch("http://example.org:8080/compute1", occi_resource:id(R)),
		  ?assertMatch({"http://schemas.ogf.org/occi/infrastructure#", "compute"}, occi_resource:kind(R)),
		  ?assertMatch(#{ "occi.core.summary" := "A super computer",
				  "occi.compute.cores" := 45,
				  "occi.compute.hostname" := "a name",
				  "occi.compute.speed" := 1.5}, 
			       occi_resource:attributes(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {basename, Basename}, {check, Fun} | Config ];

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
     {core_resource, [], [parse_xml, parse_text, parse_json]}
    ,{core_link,     [], [parse_xml, parse_text, parse_json]}
    ,{'compute_a',   [], [parse_xml, parse_text, parse_json]}
    ].


all() -> 
    [
     {group, core_resource}
    ,{group, core_link}
    ,{group, 'compute_a'}
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
    lists:foreach(fun (Filename) ->
			  parse_test(Type, Filename, Config)
		  end, filelib:wildcard(?config(basename, Config) ++ "*" ++ Ext)).


parse_test(Type, Filename, Config) ->
    ct:log(info, ?STD_IMPORTANCE, "=== Parsing ~s", [filename:basename(Filename)]),
    {ok, Bin} = file:read_file(Filename),
    R = occi_entity:load(Type, Bin, client),
    Fun = ?config(check, Config),
    Fun(R).