%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_resource).

-include("occi_entity.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [new/1, new/2, load/3]},
	occi_type]).

-export([new/1,
	 new/2,
	 add_link/2,
	 links/1]).

-export([load/3]).

-define(links, 7).

-type resource() :: {
		Class      :: occi_type:name(),
		Id         :: string(),
		Kind       :: occi_category:id(),
		Mixins     :: [occi_category:id()],
		Attributes :: maps:map(),
		Values     :: maps:map(),
		Links      :: list()
	       }.

-type t() :: resource().
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "resource"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc Creates a resource with given id, of kind ...core#resource
%% @end
-spec new(string()) -> t().
new(Id) ->
    occi_entity:new(Id, ?category_id).


%% @doc Creates a resource
%% @end
-spec new(string(), occi_category:id()) -> t().
new(Id, KindId) ->
    RList = tuple_to_list(occi_entity:new(Id, KindId)),
    list_to_tuple(RList ++ [ [] ]).


%% @doc Add the given link to the resource
%% @end
-spec add_link(occi_link:t(), occi_resource:t()) -> t().
add_link(Link, R) when element(?class, Link) =:= link ->
    L0 = element(?links, R),
    setelement(?links, R, [ Link | L0 ]).


%% @doc Get list of links associated to this resource
%% @end
-spec links(occi_resource:t()) -> [occi_entity:id()].
links(R) ->
    element(?links, R).


%% @doc Load resource from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist(), occi_entity:validation()) -> t().
load(Mimetype, Bin, V) -> 
    occi_rendering:load_entity(resource, Mimetype, Bin, V).

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
