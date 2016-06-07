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
-include("occi_log.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [from_map/2, change_prefix/3]},
	occi_type]).

-export([new/1,
	 new/2,
	 add_link/2,
	 links/1,
	 links/2]).

-export([from_map/2]).

-export([change_prefix/3]).

-define(links, 9).

-type resource() :: {
		Class      :: occi_type:name(),
		Id         :: binary(),
		Location   :: occi_uri:url(),
		Kind       :: occi_category:id(),
		Mixins     :: [occi_category:id()],
		Attributes :: maps:map(),
		Values     :: maps:map(),
		Actions    :: maps:map(),
		Links      :: list()
	       }.

-opaque t() :: resource().
-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc Creates a resource with given id, of kind ...core#resource
%% @end
-spec new(occi_entity:id()) -> t().
new(Id) ->
    occi_resource:new(Id, ?resource_kind_id).


%% @throws {unknown_category, term()}
-spec new(occi_entity:id(), occi_category:t() | occi_category:id() | string() | binary()) -> t().
new(Id, KindId) when is_list(KindId); is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId));

new(Id, {_Scheme, _Term}=KindId) ->
    new(Id, occi_models:kind(resource, KindId));

new(Id, Kind) ->
    occi_entity:merge_parents(Kind, {resource, Id, undefined, occi_kind:id(Kind), [], #{}, #{}, #{}, []}).


%% @doc Add the given link to the resource
%% @end
-spec add_link(occi_link:t(), occi_resource:t()) -> t().
add_link(Link, R) when element(?class, Link) =:= link ->
    L0 = element(?links, R),
    setelement(?links, R, [ Link | L0 ]).


%% @doc Get list of links associated to this resource
%% @end
-spec links(occi_resource:t()) -> [ occi_link:t() | occi_entity:id() ].
links(R) ->
    element(?links, R).


%% @doc Set full list of links
%% @end
-spec links([ occi_link:t() | occi_entity:id() ], occi_resource:t()) -> t().
links(Links, Resource) ->
    setelement(?links, Resource, Links).


%% @doc New resource from AST
%% @end
-spec from_map(occi_kind:t(), occi_rendering:ast()) -> t().
from_map(Kind, Map) when ?is_kind(Kind), is_map(Map) ->
    try begin
	    Id = maps:get(id, Map, undefined),
	    R = new(Id, Kind),
	    R0 = case maps:get(location, Map, undefined) of
		     undefined -> R;
		     Location -> occi_entity:location(Location, R)
		 end,
	    R1 = lists:foldl(fun (M, Acc1) ->
				     add_mixin(M, Acc1)
			     end, R0, maps:get(mixins, Map, [])),
	    R2 = lists:foldl(fun (LinkMap, Acc2) ->
				     add_link_from_map(LinkMap, Acc2, Id, Kind)
			     end, R1, maps:get(links, Map, [])),
	    Attrs0 = maps:get(attributes, Map, #{}),
	    occi_entity:set(Attrs0, client, R2)
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.


add_link_from_map(LinkMap, Acc, Id, Kind) ->
    LinkMap2 = case maps:is_key(source, LinkMap) of
		   true -> 
		       LinkMap;
		   false ->
		       LinkMap#{ source => #{ location => Id,
					      kind => occi_kind:id(Kind) } }
	       end,
    add_link(occi_link:from_map(LinkMap2), Acc).    


%% @doc Change urls prefix
%% @end
-spec change_prefix(occi_uri:prefix_op(), binary(), t()) -> t().
change_prefix(Op, Prefix, Res) ->
    Location2 = occi_uri:change_prefix(Op, Prefix, element(?location, Res)),
    Res2 = location(Location2, Res),
    Links2 = lists:foldl(fun (Link, Acc) when is_binary(Link) ->
				 [ occi_uri:change_prefix(Op, Prefix, Link) | Acc ];
			     (Link, Acc) when ?is_link(Link) ->
				 [ occi_link:change_prefix(Op, Prefix, Link) | Acc ]
			 end, [], links(Res2)),
    links(Links2, Res2).
    

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
