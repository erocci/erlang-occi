%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 25 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(models_SUITE).

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
     {attributes, [], 
      [ 
	simple_entity
      ,simple_resource
      ,mixin_resource
      ,mixin_depend_resource
      ,mixin_override_resource
      ,mixin_delete_resource1
      ,mixin_delete_resource2
      ]}
    ].


all() -> 
    [
     {group, attributes}
    ].

%%%
%%% Tests
%%%
-define(entity_id, "http://example.org/entity").

-define(mixin0_xml, <<"<?xml version=\"1.0\" ?>"
		      "<mixin xmlns=\"http://schemas.ogf.org/occi\""
		      "   xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" "
		      "   term=\"mixin0\" scheme=\"http://schemas.example.org/occi#\" >"
		      "  <attribute name=\"occi.mixin.attr0\" type=\"xs:string\" />" 
		      "</mixin>">>).

-define(mixin1_xml, <<"<?xml version=\"1.0\" ?>"
		      "<mixin xmlns=\"http://schemas.ogf.org/occi\""
		      "   xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" "
		      "   term=\"mixin1\" scheme=\"http://schemas.example.org/occi#\" >"
		      "  <depends term=\"mixin0\" scheme=\"http://schemas.example.org/occi#\" />"
		      "  <attribute name=\"occi.mixin.attr1\" type=\"xs:string\" default=\"Default value 0\" />" 
		      "</mixin>">>).

%% depends on mixin1, change default of attr1
-define(mixin2_xml, <<"<?xml version=\"1.0\" ?>"
		      "<mixin xmlns=\"http://schemas.ogf.org/occi\""
		      "   xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" "
		      "   term=\"mixin2\" scheme=\"http://schemas.example.org/occi#\" >"
		      "  <depends term=\"mixin1\" scheme=\"http://schemas.example.org/occi#\" />"
		      "  <attribute name=\"occi.mixin.attr1\" type=\"xs:string\" default=\"Default value 1\" />" 
		      "</mixin>">>).

simple_entity(_Config) ->
    E = occi_entity:new(?entity_id),
    ?assertMatch(undefined, occi_entity:get("occi.core.title", E)).


simple_resource(_Config) ->
    R = occi_resource:new(?entity_id),
    ?assertMatch(undefined, occi_resource:get("occi.core.title", R)),
    ?assertMatch(undefined, occi_resource:get("occi.core.summary", R)),
    ?assertThrow({invalid_key, "bad"}, occi_resource:get("bad", R)).

mixin_resource(_Config) ->
    M = occi_parser_xml:parse(mixin, ?mixin0_xml),
    ok = occi_models:add_category(M),
    R = occi_resource:new(?entity_id),
    R1 = occi_resource:add_mixin({"http://schemas.example.org/occi#", "mixin0"}, R),
    ?assertMatch(undefined, occi_resource:get("occi.core.title", R1)),
    ?assertMatch(undefined, occi_resource:get("occi.core.summary", R1)),
    ?assertMatch(undefined, occi_resource:get("occi.mixin.attr0", R1)).

mixin_depend_resource(_Config) ->
    M = occi_parser_xml:parse(mixin, ?mixin1_xml),
    ok = occi_models:add_category(M),
    R = occi_resource:new(?entity_id),
    R1 = occi_resource:add_mixin({"http://schemas.example.org/occi#", "mixin1"}, R),
    ?assertMatch(undefined, occi_resource:get("occi.core.title", R1)),
    ?assertMatch(undefined, occi_resource:get("occi.core.summary", R1)),
    ?assertMatch(undefined, occi_resource:get("occi.mixin.attr0", R1)),
    ?assertMatch("Default value 0", occi_resource:get("occi.mixin.attr1", R1)).


mixin_override_resource(_Config) ->
    M = occi_parser_xml:parse(mixin, ?mixin2_xml),
    ok = occi_models:add_category(M),
    R = occi_resource:new(?entity_id),
    R1 = occi_resource:add_mixin({"http://schemas.example.org/occi#", "mixin2"}, R),
    ?assertMatch(undefined, occi_resource:get("occi.core.title", R1)),
    ?assertMatch(undefined, occi_resource:get("occi.core.summary", R1)),
    ?assertMatch(undefined, occi_resource:get("occi.mixin.attr0", R1)),
    ?assertMatch("Default value 1", occi_resource:get("occi.mixin.attr1", R1)).


%% @doc Test default value of an attribute when removing a mixin who defined
%% an already defined attribute, but with different default value
%% @end
mixin_delete_resource1(_Config) ->
    M = occi_parser_xml:parse(mixin, ?mixin2_xml),
    ok = occi_models:add_category(M),
    R = occi_resource:new(?entity_id),
    R1 = occi_resource:add_mixin({"http://schemas.example.org/occi#", "mixin2"}, R),
    ?assertMatch("Default value 1", occi_resource:get("occi.mixin.attr1", R1)),
    R2 = occi_resource:rm_mixin({"http://schemas.example.org/occi#", "mixin2"}, R1),
    ?assertMatch("Default value 0", occi_resource:get("occi.mixin.attr1", R2)).


%% @doc Test value of an attribute when removing a mixin 
%% @end
mixin_delete_resource2(_Config) ->
    M = occi_parser_xml:parse(mixin, ?mixin2_xml),
    ok = occi_models:add_category(M),
    R = occi_resource:new(?entity_id),
    R1 = occi_resource:add_mixin({"http://schemas.example.org/occi#", "mixin2"}, R),
    ?assertMatch("Default value 1", occi_resource:get("occi.mixin.attr1", R1)),
    R2 = occi_resource:set("occi.mixin.attr1", "custom value", R1),
    R3 = occi_resource:rm_mixin({"http://schemas.example.org/occi#", "mixin2"}, R2),
    ?assertMatch("custom value", occi_resource:get("occi.mixin.attr1", R3)).
    
