%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(renderers_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ctx, occi_uri:from_string(<<"http://example.org:8080/">>)).

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


init_per_group('categories', Config) ->
    Object = occi_models:categories(),
    [ {object, Object}, {ctx, ?ctx} | Config ];

init_per_group('core_resource', Config) ->
    Id = <<"myresource">>,
    R0 = occi_resource:new(Id, {<<"http://schemas.ogf.org/occi/core#">>, <<"resource">>}),
    R1 = occi_resource:location(<< $/, Id/binary >>, R0),
    [ {object, R1}, {ctx, ?ctx} | Config ];

init_per_group('compute_a', Config) ->
    Id = <<"ns1/mycompute0">>,
    R = occi_resource:new(Id, {<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>}),
    R0 = occi_resource:add_mixin({<<"http://occi.example.org/occi/infrastructure/os_tpl#">>, <<"debian6">>}, R),
    R1 = occi_resource:set(#{ <<"occi.core.title">> => <<"My super compute">>,
			      <<"occi.compute.cores">> => 4,
			      <<"occi.compute.hostname">> => <<"mycompute">>,
			      <<"occi.compute.speed">> => 4.5,
			      <<"occi.compute.memory">> => 2.5 }, client, R0),
    R2 = occi_resource:location(<< $/, Id/binary >>, R1),
    [ {object, R2}, {ctx, ?ctx} | Config ];

init_per_group('compute_b', Config) ->
    RId = <<"ns1/mycompute0">>,
    RKind = {<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>},
    R = occi_resource:new(RId, RKind),
    LId = <<"myif0">>,
    L = occi_link:new(LId, {<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>}, 
		      RId, RKind, 
		      <<"network0">>, undefined),
    R1 = occi_resource:add_link(L, R),
    R2 = occi_resource:set(#{ <<"occi.compute.cores">> => 4 }, client, R1),
    R3 = occi_resource:location(<< $/, RId/binary >>, R2),
    [ {object, R3}, {ctx, ?ctx} | Config ];

init_per_group('netif', Config) ->
    Id = <<"myif">>,
    Kind = {<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"networkinterface">>},
    Mixin = {<<"http://schemas.ogf.org/occi/infrastructure/networkinterface#">>, <<"ipnetworkinterface">>},
    Src = <<"mycompute">>,
    SrcKind = {<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"compute">>},
    Target = <<"mynetwork">>,
    TargetKind = {<<"http://schemas.ogf.org/occi/infrastructure#">>, <<"network">>},
    L = occi_link:new(Id, Kind, Src, SrcKind, Target, TargetKind),
    L0 = occi_link:add_mixin(Mixin, L),
    L1 = occi_link:set(#{ <<"occi.networkinterface.interface">> => <<"eth0">>,
			  <<"occi.networkinterface.mac">> => <<"00:11:22:33:44:55">>,
			  <<"occi.networkinterface.address">> => <<"192.168.0.1">>,
			  <<"occi.networkinterface.allocation">> => <<"static">>}, client, L0),
    L2 = occi_link:location(<< $/, Id/binary >>, L1),
    [ {object, L2}, {ctx, ?ctx} | Config ];

init_per_group('bounded_collection', Config) ->
    Kind = {<<"http://schemas.ogf.org/occi/core#">>, <<"resource">>},
    C = occi_collection:new(Kind),
    R0 = occi_resource:location(<<"/resource0">>, occi_resource:new(<<"resource0">>)),
    R1 = occi_resource:location(<<"/resource1">>, occi_resource:new(<<"resource1">>)),
    C1 = occi_collection:append([R0, R1], C),
    [ {object, C1}, {ctx, ?ctx} | Config ];

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
     {'categories',          [], [render_xml, render_text, render_json]}
    ,{'core_resource',       [], [render_xml, render_text, render_json]}
    ,{'compute_a',           [], [render_xml, render_text, render_json]}
    ,{'compute_b',           [], [render_xml, render_text, render_json]}
    ,{'netif',               [], [render_xml, render_text, render_json]}
    ,{'bounded_collection',  [], [render_xml, render_text, render_json, render_uri]}
    ].


all() -> 
    [
     {group, 'categories'}
    ,{group, 'core_resource'}
    ,{group, 'compute_a'}
    ,{group, 'compute_b'}
    ,{group, 'netif'}
    ,{group, 'bounded_collection'}
    ].


%%%
%%% Tests
%%%
render_text(Config) -> render_test(text, ".occi", Config).

render_uri(Config) -> render_test('uri-list', ".uri", Config).

render_xml(Config) -> render_test(xml, ".xml", Config).

render_json(Config) -> render_test(json, ".json", Config).


%%%
%%% Internal
%%%
render_test(Type, Ext, Config) ->
    Basename = atom_to_list(proplists:get_value(name, ?config(tc_group_properties, Config))) ++ Ext,
    Filename = filename:join([?config(data_dir, Config), Basename]),
    ct:log(info, ?STD_IMPORTANCE, "=== Check rendering: ~s", [Basename]),
    {ok, Match} = file:read_file(Filename),
    Rendered = iolist_to_binary(occi_rendering:render(Type, ?config(object, Config), ?config(ctx, Config))),
    CleanMatch = strip(Match),
    CleanRendered = strip(Rendered),
    case binary_match(CleanMatch, CleanRendered) of
	true ->
	    ?assert(true);
	false ->
	    ct:pal(error, ?STD_IMPORTANCE, "=== Binary match error: ~s~n", [Basename]),
	    ct:pal(error, ?STD_IMPORTANCE, "Expected :~n~n#BEGIN#~s#END#~n~n", [CleanMatch]),
	    ct:pal(error, ?STD_IMPORTANCE, "Rendered:~n~n#BEGIN#~s#END#~n", [CleanRendered]),
	    ?assert(false)
    end.


binary_match(<<>>, <<>>) ->
    true;

binary_match(<< C, Rest0/binary >>, << C, Rest1/binary >>) ->
    binary_match(Rest0, Rest1);

binary_match(_, _) ->
    false.


-define(is_ws(X), X =:= $\s; X =:= $\t; X =:= $\r; X =:= $\n).
strip(<<>>) ->
    <<>>;

strip(<< C, Rest/binary >>) when ?is_ws(C) ->
    strip(Rest);

strip(<< C, Rest/binary >>) ->
    strip2(Rest, << C >>).


strip2(<<>>, Acc) ->
    strip3(Acc);

strip2(<< C, Rest/binary >>, Acc) ->
    strip2(Rest, << C, Acc/binary >>).


strip3(<< C, Rest/binary >>) when ?is_ws(C) ->
    strip3(Rest);

strip3(<< C, Rest/binary >>) ->
    strip4(Rest, << C >>).


strip4(<<>>, Acc) ->
    Acc;

strip4(<< C, Rest/binary >>, Acc) ->
    strip4(Rest, << C, Acc/binary >>).
