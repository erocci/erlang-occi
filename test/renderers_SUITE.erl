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

-define(ctx, uri:new(<<"http">>, <<"">>, <<"example.org">>, 8080, <<"/">>, [], <<"">>)).

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
    Object = occi_resource:new("myresource", {"http://schemas.ogf.org/occi/core#", "resource"}),
    Ctx = ?ctx,
    [ {basename, Basename}, {object, Object}, {ctx, Ctx} | Config ];

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
     {core_resource,       [], [render_xml]}
    ].


all() -> 
    [
     {group, core_resource}
    ].


%%%
%%% Tests
%%%
render_text(Config) -> render_test(text, ".occi", Config).

render_xml(Config) -> render_test(xml, ".xml", Config).

render_json(Config) -> render_test(json, ".json", Config).


%%%
%%% Internal
%%%
render_test(Type, Ext, Config) ->
    Filename = ?config(basename, Config) ++ Ext,
    ct:log(info, ?STD_IMPORTANCE, "=== Check rendering: ~s", [filename:basename(Filename)]),
    {ok, Match} = file:read_file(Filename),
    Rendered = iolist_to_binary(occi_rendering:render(Type, ?config(object, Config), ?config(ctx, Config))),
    CleanMatch = strip(Match),
    CleanRendered = strip(Rendered),
    case binary_match(CleanMatch, CleanRendered) of
	true ->
	    ?assert(true);
	false ->
	    ct:pal(error, ?STD_IMPORTANCE, "=== Binary match error:~n", []),
	    ct:pal(error, ?STD_IMPORTANCE, "Expected:~n~n#BEGIN#~s#END#~n~n", [CleanMatch]),
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
    strip4(Rest, << Acc/binary, C >>).
