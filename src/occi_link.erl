%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_link).

-include("occi_entity.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [new/1, new/2]},
	occi_type]).

-export([new/3, 
	 new/4,
	 source/1,
	 target/1]).

-export([load/3]).

-type t() :: occi_entity:t().
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "link"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec new(string(), string(), string()) -> t().
new(Id, Src, Target) ->
    new(Id, ?category_id, Src, Target).


%% @throws {unknown_category, term()}
-spec new(string(), occi_category:id() | string() | binary(), string(), string()) -> t().
new(Id, KindId, Src, Target) when is_list(Id), is_list(Src), is_list(Target) ->
    Link = occi_entity:new(Id, KindId),
    set(#{ "occi.core.source" => Src, "occi.core.target" => Target }, internal, Link).


-spec source(t()) -> string().
source(E) ->
    get("occi.core.source", E).


-spec target(t()) -> string().
target(E) ->
    get("occi.core.target", E).


%% @doc Load link from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist(), occi_entity:validation()) -> t().
load(Mimetype, Bin, V) -> 
    occi_rendering:load_entity(link, Mimetype, Bin, V).
