%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(render_xml_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ctx, uri:new(<<"http">>, <<"">>, <<"example.org">>, 8080, <<"/">>, [], <<"">>)).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(occi),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(occi),
    ok = application:stop(mnesia),
    ok.


init_per_group(entities, Config) ->
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    E = occi_extension:load_path(ExtFile),
    ok = occi_models:import(E),
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
     {extension, [], [render_extension]}
    ,{entities, [], 
      [
       render_resource
      ,render_resource2
      ,render_compute1
      ]}
    ].


all() -> 
    [{group, extension}, {group, entities}].


render_extension(Config) -> 
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    E = occi_extension:load_path(ExtFile),
    Out = occi_rendering:render(xml, E, ?ctx),
    ?assertMatch(["<?xml version=\"1.0\"?>" | _], Out),
    ok.


render_resource(_Config) ->
    R = occi_resource:new("ns1/myresource0", {"http://schemas.ogf.org/occi/core#", "resource"}),
    R1 = occi_resource:set("occi.core.title", "My super title", R),
    ?assertMatch(<<"<?xml version=\"1.0\"?>"
		   "<resource id=\"ns1/myresource0\" href=\"http://example.org:8080/ns1/myresource0\" "
		   "title=\"My super title\" "
		   "xmlns=\"http://schemas.ogf.org/occi\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">"
		   "<kind scheme=\"http://schemas.ogf.org/occi/core#\" term=\"resource\"/>"
		   "</resource>">>, 
		 iolist_to_binary(occi_renderer_xml:render(R1, ?ctx))).


render_resource2(_Config) ->
    R = occi_resource:new("ns1/myresource0", {"http://schemas.ogf.org/occi/core#", "resource"}),
    R1 = occi_resource:set("occi.core.title", "My super title", R),
    R2 = occi_resource:set("occi.core.summary", "My super summary", R1),
    ?assertMatch(<<"<?xml version=\"1.0\"?>"
		   "<resource id=\"ns1/myresource0\" href=\"http://example.org:8080/ns1/myresource0\" "
		   "title=\"My super title\" "
		   "xmlns=\"http://schemas.ogf.org/occi\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">"
		   "<kind scheme=\"http://schemas.ogf.org/occi/core#\" term=\"resource\"/>"
		   "<attribute name=\"occi.core.summary\" value=\"My super summary\"/>"
		   "</resource>">>, 
		 iolist_to_binary(occi_renderer_xml:render(R2, ?ctx))).


render_compute1(_Config) ->
    R = occi_resource:new("ns1/mycompute0", {"http://schemas.ogf.org/occi/infrastructure#", "compute"}),
    R0 = occi_resource:add_mixin({"http://occi.example.org/occi/infrastructure/os_tpl#", "debian6"}, R),
    R1 = occi_resource:set("occi.core.title", "My super compute", R0),
    R2 = occi_resource:set("occi.compute.cores", 4, R1),
    R3 = occi_resource:set("occi.compute.hostname", "mycompute", R2),
    R4 = occi_resource:set("occi.compute.speed", 4.5, R3),
    R5 = occi_resource:set("occi.compute.memory", 2.5, R4),
    ?assertMatch(<<"<?xml version=\"1.0\"?>"
		   "<resource id=\"ns1/mycompute0\" href=\"http://example.org:8080/ns1/mycompute0\" "
		   "title=\"My super compute\" "
		   "xmlns=\"http://schemas.ogf.org/occi\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">"
		   "<kind scheme=\"http://schemas.ogf.org/occi/infrastructure#\" term=\"compute\"/>"
		   "<mixin scheme=\"http://occi.example.org/occi/infrastructure/os_tpl#\" term=\"debian6\"/>"
		   "<attribute name=\"occi.compute.cores\" value=\"4\"/>"
		   "<attribute name=\"occi.compute.hostname\" value=\"mycompute\"/>"
		   "<attribute name=\"occi.compute.memory\" value=\"2.5\"/>"
		   "<attribute name=\"occi.compute.speed\" value=\"4.5\"/>"
		   "</resource>">>, 
		 iolist_to_binary(occi_renderer_xml:render(R5, ?ctx))).
