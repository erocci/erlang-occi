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
    [render_extension].


render_extension(Config) -> 
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    E = occi_extension:load_path(ExtFile),
    Out = occi_rendering:render(xml, E, ?ctx),
    ?assertMatch(["<?xml version=\"1.0\"?>" | _], Out),
    ok.
