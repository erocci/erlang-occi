%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_resource).

-include("occi.hrl").
-include("occi_entity.hrl").
-include("occi_uri.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [from_map/2]},
	occi_type]).

-export([new/1,
	 new/2,
	 add_link/2,
	 links/1]).

-export([from_map/2]).

-define(links, 8).

-type resource() :: {
		Class      :: occi_type:name(),
		Id         :: binary(),
		Kind       :: occi_category:id(),
		Mixins     :: [occi_category:id()],
		Attributes :: maps:map(),
		Values     :: maps:map(),
		Actions    :: maps:map(),
		Links      :: list()
	       }.

-type t() :: resource().
-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc Creates a resource with given id, of kind ...core#resource
%% @end
-spec new(binary()) -> t().
new(Id) ->
    occi_resource:new(Id, ?resource_kind_id).


%% @throws {unknown_category, term()}
-spec new(binary(), occi_category:t() | occi_category:id() | string() | binary()) -> t().
new(Id, KindId) when is_list(KindId); is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId));

new(Id, {_Scheme, _Term}=KindId) ->
    new(Id, occi_models:kind(resource, KindId));

new(Id, Kind) ->
    occi_entity:merge_parents(Kind, {resource, Id, occi_kind:id(Kind), [], #{}, #{}, #{}, []}).


%% @doc Add the given link to the resource
%% @end
-spec add_link(occi_link:t(), occi_resource:t()) -> t().
add_link(Link, R) when element(?class, Link) =:= link ->
    L0 = element(?links, R),
    setelement(?links, R, [ Link | L0 ]).


%% @doc Get list of links associated to this resource
%% @end
-spec links(occi_resource:t()) -> [occi_link:t()].
links(R) ->
    element(?links, R).


%% @doc New resource from AST
%% @end
-spec from_map(occi_kind:t(), occi_rendering:ast()) -> t().
from_map(Kind, Map) when ?is_kind(Kind), is_map(Map) ->
    try begin
	    Id = maps:get(id, Map, undefined),
	    R = new(Id, Kind),
	    R1 = lists:foldl(fun (M, Acc1) ->
				     add_mixin(M, Acc1)
			     end, R, maps:get(mixins, Map, [])),
	    R2 = lists:foldl(fun (Map2, Acc2) ->
				     add_link(occi_link:from_map(Map2), Acc2)
			     end, R1, maps:get(links, Map, [])),
	    Attrs0 = maps:get(attributes, Map, #{}),
	    Attrs1 = Attrs0#{ <<"occi.core.summary">> => maps:get(summary, Map, undefined),
			      <<"occi.core.title">> => maps:get(title, Map, undefined) },
	    occi_entity:set(Attrs1, client, R2)
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
