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
	 mimetype/1]).

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
mimetype(Path) ->
    case filename:extension(Path) of
	".xml" ->
	    {<<"application">>, <<"xml">>, []};
	".json" ->
	    {<<"application">>, <<"json">>, []}
    end.
