%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_utils).

-include("occi_log.hrl").

-export([mkdir/1,
	 priv_dir/0,
	 resources_dir/0,
	 mimetype/1,
	 urn/1,
	 ctx/2]).

-type mimetype() :: {Type :: binary(), SubType :: binary(), Options :: list()}
		  | undefined.

-export_type([mimetype/0]).


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


-spec mkdir(file:filename_all()) -> ok.
mkdir(Dir) ->
    case filelib:is_dir(Dir) of
	true ->
	    ok;
	false ->
	    ?debug("Creates directory: ~s", [Dir]),
	    file:make_dir(Dir)
    end.


-spec mimetype(file:filename_all()) -> mimetype().
mimetype(Path) ->
    case filename:extension(Path) of
	".xml" ->
	    {<<"application">>, <<"xml">>, []};
	".json" ->
	    {<<"application">>, <<"json">>, []}
    end.


-spec urn(binary()) -> binary().
urn(Seed) ->
    <<"urn:uuid:", (uuid:uuid_to_string(uuid:get_v3(oid, Seed), binary_standard))/binary >>.


-spec ctx(string() | binary(), occi_rendering:ctx()) -> string() | binary().
ctx(Path, Ctx) when is_list(Path) ->
    binary_to_list(ctx(list_to_binary(Path), Ctx));

ctx(Path, Ctx) ->
    uri:to_string(uri:path(Ctx, filename:join([uri:path(Ctx), Path]))).
