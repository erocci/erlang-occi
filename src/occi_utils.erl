%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_utils).

-include("occi_log.hrl").

-export([is_absolute/1,
	 normalize/1,
	 mkdir/1,
	 priv_dir/0,
	 resources_dir/0,
	 mimetype/1,
	 normalize_mimetype/1]).

-type mimetype() :: {Type :: binary(), SubType :: binary(), Options :: list()}
		  | undefined.

-export_type([mimetype/0]).


%% @doc Normalize path: remove duplicate and trailing '/'
%% @end
-spec normalize(binary()) -> binary().
normalize(Path) ->
    << <<$/, Segment/binary >> || Segment <- (binary:split(Path, [<<$/>>], [global, trim_all])) >>.


-spec is_absolute(binary()) -> boolean().
is_absolute(<< $/, _ >>) -> true;

is_absolute(Path) when is_binary(Path) -> false.


-spec priv_dir() -> file:filename_all().
priv_dir() ->
    case code:priv_dir(occi) of
	{error, bad_name} ->
	    filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
	Dir ->
	    Dir
    end.


-spec resources_dir() -> file:filename_all().
resources_dir() ->
    case application:get_env(occi, resources_dir, undefined) of
	undefined ->
	    throw({undefined_env, resources_dir});
	{priv_dir, Dir} ->
	    filename:join([priv_dir(), Dir]);
	Dir when is_list(Dir) ->
	    Dir
    end.


%% @doc Recursively creates dir
%% @end
-spec mkdir(file:filename_all()) -> ok | {error, file:posix() | badarg}.
mkdir(Dir) ->
    case filelib:is_dir(Dir) of
	true -> ok;
	false ->
	    case mkdir(filename:dirname(Dir)) of
		ok ->
		    ?debug("Creates directory: ~s", [Dir]),
		    file:make_dir(Dir);
		{error, _}=Err ->
		    Err
	    end
    end.


-spec mimetype(file:filename_all()) -> mimetype().
mimetype(Path) when is_list(Path) ->
    mimetype(list_to_binary(Path));

mimetype(Path) ->
    case filename:extension(Path) of
	<<".xml">> ->
	    {<<"application">>, <<"xml">>, []};
	<<".json">> ->
	    {<<"application">>, <<"json">>, []};
	Ext ->
	    throw({unknown_mimetype, Ext})
    end.


-define(mimetype_plain,    {<<"text">>, <<"occi+plain">>, []}).
-define(mimetype_plain(V), {<<"text">>, <<"occi+plain">>, V}).
-define(mimetype_occi,     {<<"text">>, <<"occi">>, []}).
-define(mimetype_occi(V),  {<<"text">>, <<"occi">>, V}).
-define(mimetype_uri,      {<<"text">>, <<"uri-list">>, []}).
-define(mimetype_uri(V),   {<<"text">>, <<"uri-list">>, V}).
-define(mimetype_xml,      {<<"application">>, <<"occi+xml">>, []}).
-define(mimetype_xml(V),   {<<"application">>, <<"occi+xml">>, V}).
-define(mimetype_json,     {<<"application">>, <<"occi+json">>, []}).
-define(mimetype_json(V),  {<<"application">>, <<"occi+json">>, V}).


-spec normalize_mimetype(term()) -> occi_utils:mimetype().
normalize_mimetype(undefined)                                -> ?mimetype_plain;
normalize_mimetype(<<"*/*">>)                                -> ?mimetype_plain;
normalize_mimetype(<<"application/xml">>)                    -> ?mimetype_xml;
normalize_mimetype(<<"application/occi+xml">>)               -> ?mimetype_xml;
normalize_mimetype({<<"application">>, <<"xml">>, V})        -> ?mimetype_xml(V);
normalize_mimetype({<<"application">>, <<"occi+xml">>, V})   -> ?mimetype_xml(V);
normalize_mimetype(xml)                                      -> ?mimetype_xml;
normalize_mimetype(<<"application/json">>)                   -> ?mimetype_json;
normalize_mimetype(<<"application/occi+json">>)              -> ?mimetype_json;
normalize_mimetype({<<"application">>, <<"json">>, V})       -> ?mimetype_json(V);
normalize_mimetype({<<"application">>, <<"occi+json">>, V})  -> ?mimetype_json(V);
normalize_mimetype(json)                                     -> ?mimetype_json;
normalize_mimetype(<<"text/plain">>)                         -> ?mimetype_plain;
normalize_mimetype(<<"text/occi+plain">>)                    -> ?mimetype_plain;
normalize_mimetype(<<"text/occi">>)                          -> ?mimetype_occi;
normalize_mimetype({<<"text">>, <<"plain">>, V})             -> ?mimetype_plain(V);
normalize_mimetype({<<"text">>, <<"occi+plain">>, V})        -> ?mimetype_plain(V);
normalize_mimetype({<<"text">>, <<"occi">>, V})              -> ?mimetype_occi(V);
normalize_mimetype(text)                                     -> ?mimetype_plain;
normalize_mimetype(<<"text/uri-list">>)                      -> ?mimetype_uri;
normalize_mimetype({<<"text">>, <<"uri-list">>, V})          -> ?mimetype_uri(V);
normalize_mimetype('uri-list')                               -> ?mimetype_uri.
