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
    [parse_extension].


parse_extension(Config) -> 
    ExtFile = filename:join([?config(data_dir, Config), "occi-infrastructure.xml"]),
    {ok, Xml} = file:read_file(ExtFile),
    Ext = occi_parser_xml:parse(occi_extension, Xml),
    ct:log(info, "extension: ~p", [Ext]),
    ?assertMatch("Infrastructure", occi_extension:name(Ext)),
    ?assertMatch("http://schemas.ogf.org/occi/infrastructure#", occi_extension:scheme(Ext)),
    ?assertEqual(5, length(occi_extension:kinds(Ext))),
    ?assertEqual(6, length(occi_extension:mixins(Ext))),
    ok.
