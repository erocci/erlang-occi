%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_test).


-compile(export_all).

test() ->
    {ok, _} = application:ensure_all_started(occi),
    ExtFile = filename:join([test_dir(), "load_extension_SUITE_data", "occi-infrastructure.xml"]),
    E = occi_extension:load_path(ExtFile),
    Out = occi_rendering:render(xml, E),
    io:format("~s~n", [lists:flatten(Out)]).



%%%
%%%
%%%
test_dir() ->
    filename:join([filename:dirname(code:which(?MODULE)), "..", "test"]).
