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
    Ext = occi_rendering:parse_file(ExtFile, occi_extension),
    {ok, _} = occi_models:import(Ext),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(occi),
    ok = application:stop(mnesia),
    ok.


init_per_group(core_resource, Config) ->
    Fun = fun(R)  ->
		  ?assertMatch(<<"resource1">>, occi_resource:id(R)),
		  ?assertMatch(undefined, occi_resource:location(R)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/core#">>, <<"resource">>}, occi_resource:kind(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group(core_link, Config) ->
    Fun = fun(L)  ->
		  ?assertMatch(<<"link1">>, occi_link:id(L)),
		  ?assertMatch(undefined, occi_link:location(L)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/core#">>, <<"link">>}, occi_link:kind(L)),
		  ?assertMatch(<<"/myresource0">>, occi_link:source(L)),
		  ?assertMatch(<<"/myresource1">>, occi_link:target(L)),
		  ?assertMatch(#{}, occi_link:attributes(L))
	  end,
    [ {check, Fun} | Config ];

init_per_group('netif_link', Config) ->
    Fun = fun(L)  ->
		  ?assertMatch(<<"netif1">>, occi_link:id(L)),
		  ?assertMatch(undefined, occi_link:location(L)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>}, 
			       occi_link:kind(L)),
		  ?assertMatch([{<<"http://schemas.ogf.org/occi/infrastructure/networkinterface#">>, <<"ipnetworkinterface">>}], 
			       occi_link:mixins(L)),
		  ?assertMatch(#{ <<"occi.networkinterface.interface">> := <<"eth0">>,
				  <<"occi.networkinterface.mac">> := <<"00:11:22:33:44:55">>,
				  <<"occi.networkinterface.address">> := <<"192.168.0.1">>,
				  <<"occi.networkinterface.allocation">> := static }, 
			       occi_link:attributes(L)),
		  ?assertMatch(<<"/compute1">>, occi_link:source(L)),
		  ?assertMatch(<<"/network1">>, occi_link:target(L))
	  end,
    [ {check, Fun} | Config ];

init_per_group('resource_link', Config) ->
    Fun = fun(R)  ->
		  ?assertMatch(<<"resource1">>, occi_resource:id(R)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/core#">>, <<"resource">>}, occi_resource:kind(R)),
		  [Link] = occi_resource:links(R),
 		  ?assertMatch(<<"mylink1">>, occi_link:id(Link)),
 		  ?assertMatch({<<"http://schemas.ogf.org/occi/core#">>, <<"link">>}, occi_link:kind(Link)),
 		  ?assertMatch(<<"http://example.org/another_resource1">>, occi_link:target(Link)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group('compute_a', Config) ->
    Fun = fun(R)  ->
		  ?assertMatch(<<"compute1">>, occi_resource:id(R)),
		  ?assertMatch(undefined, occi_resource:location(R)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}, occi_resource:kind(R)),
		  ?assertMatch(#{ <<"occi.core.summary">> := <<"A super computer">>,
				  <<"occi.compute.cores">> := 45,
				  <<"occi.compute.hostname">> := <<"a name">>,
				  <<"occi.compute.speed">> := 1.5,
				  <<"occi.compute.state">> := inactive }, 
			       occi_resource:attributes(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group('compute_a_comma', Config) ->
    Fun = fun(R)  ->
		  ?assertMatch(<<"compute1">>, occi_resource:id(R)),
		  ?assertMatch(undefined, occi_resource:location(R)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}, occi_resource:kind(R)),
		  ?assertMatch(#{ <<"occi.core.summary">> := <<"A super, virtual computer">>,
				  <<"occi.compute.cores">> := 45,
				  <<"occi.compute.hostname">> := <<"a name">>,
				  <<"occi.compute.speed">> := 1.5,
				  <<"occi.compute.state">> := inactive }, 
			       occi_resource:attributes(R)),
		  ?assertMatch(#{}, occi_resource:attributes(R))
	  end,
    [ {check, Fun} | Config ];

init_per_group('user_mixin', Config) ->
    Fun = fun(M)  ->
		  ?assertMatch({<<"http://schemas.example.org/occi#">>, <<"mymixin0">>},
			       occi_mixin:id(M))
	  end,
    [ {check, Fun}, {occi_type, occi_mixin} | Config ];

init_per_group('user_mixin_b', Config) ->
    Fun = fun(M)  ->
		  ?assertMatch({<<"http://example.org/occi/my_stuff#">>, <<"stufik">>},
			       occi_mixin:id(M))
	  end,
    [ {check, Fun}, {occi_type, occi_mixin} | Config ];

init_per_group('collection', Config) ->
    Fun = fun(C)  ->
		  ?assertMatch([<<"/resource0">>, <<"/resource1">>],
			       occi_collection:locations(C))
	  end,
    [ {check, Fun}, {occi_type, occi_collection} | Config ];

init_per_group('invoke', Config) ->
    Fun = fun(Action)  ->
		  ?assertMatch(#{ <<"method">> := <<"graceful">> },
			       occi_invoke:attributes(Action)),
		  ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure/compute/action#">>, <<"stop">>},
			       occi_invoke:id(Action))
	  end,
    [ {check, Fun}, {occi_type, occi_invoke} | Config ];

init_per_group('invoke_b', Config) ->
    Fun = fun(Action)  ->
		  ?assertMatch({<<"http://schemas.ogf.org/occi/infrastructure/compute/action#">>, <<"start">>},
			       occi_invoke:id(Action))
	  end,
    [ {check, Fun}, {occi_type, occi_invoke} | Config ];

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
    ,{'compute_a_comma',   [], [parse_text]}
    ,{'netif_link',        [], [parse_xml, parse_text, parse_json]}
    ,{'user_mixin',        [], [parse_xml, parse_json, parse_text]}
    ,{'user_mixin_b',      [], [parse_text]}
    ,{'collection',        [], [parse_xml, parse_json, parse_text]}
    ,{'invoke',            [], [parse_xml, parse_json, parse_text]}
    ,{'invoke_b',          [], [parse_text]}
    ].


all() -> 
    [
     {group, core_resource}
    ,{group, core_link}
    ,{group, 'resource_link'}
    ,{group, 'compute_a'}
    ,{group, 'compute_a_comma'}
    ,{group, 'netif_link'}
    ,{group, 'user_mixin'}
    ,{group, 'collection'}
    ,{group, 'invoke'}
    ,{group, 'invoke_b'}
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
    Filename0 = filename:join([?config(data_dir, Config), Basename]) ++ Ext,
    case filelib:is_file(Filename0) of
	true ->
	    parse_test(Type, Filename0, Config);
	false ->
	    Filenames = filelib:wildcard(filename:join([?config(data_dir, Config), Basename]) ++ "[0-9][0-9]" ++ Ext),
	    lists:foreach(fun (Filename) ->
				  parse_test(Type, Filename, Config)
			  end, Filenames)
    end.


parse_test(Type, Filename, Config) ->
    ct:pal(info, ?STD_IMPORTANCE, "=== Parsing ~s", [filename:basename(Filename)]),
    {ok, Bin} = file:read_file(Filename),
    O = occi_rendering:parse(Type, Bin, proplists:get_value(occi_type, Config, occi_entity)),
    CheckFun = ?config(check, Config),
    CheckFun(O).
