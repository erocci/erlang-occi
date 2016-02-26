%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_resource).

-include("occi_entity.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [new/1, get/2, set/3]}]).

-export([new/1,
	 summary/1,
	 summary/2,
	 get/2,
	 set/3]).

-type t() :: occi_entity:t().
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "resource"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec new(string()) -> t().
new(Id) ->
    occi_entity:new(Id, ?category_id).

-spec summary(t()) -> string().
summary(E) ->
    ?g("occi.core.summary", E).


-spec summary(string() | binary(), t()) -> t().
summary(Summary, E) when is_binary(Summary) ->
    summary(binary_to_list(Summary), E);

summary(Summary, E) when is_list(Summary) ->
    ?s("occi.core.summary", Summary, E).


get("summary", E) ->
    get("occi.core.summary", E);

get(Key, Value) ->
    occi_entity:get(Key, Value).


set("summary", Value, E) ->
    set("occi.core.summary", Value, E);

set(Key, Value, E) ->
    occi_entity:set(Key, Value, E).


%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
