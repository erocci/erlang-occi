%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 24 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_collection).

-include("occi_type.hrl").
-include("occi_log.hrl").

-export([new/0,
	 new/1,
	 new/2,
	 id/1,
	 ids/1,
	 elements/1,
	 elements/2,
	 size/1,
	 append/2]).

-export([from_map/1,
	 from_map/2,
	 load/3,
	 render/3]).

-type id() :: binary() | occi_category:id().
-type elem() :: {occi_uri:url(), occi_entity:t() | undefined}.

-record(collection, {id                    :: id(),
		     elements = ordsets:new() :: ordsets:ordset()}).

-type t() :: #collection{}.


%% @doc Create a new collection
%% @end
-spec new() -> t().
new() ->
    #collection{id=undefined, elements=ordsets:new()}.


%% @doc Create a new collection.
%% If id is an uri, collection is unbounded.
%% If id is a category id, collection is bounded
%% @end
-spec new(binary() | occi_category:id()) -> t().
new(Id) when is_binary(Id) ->
    #collection{id=Id, elements=ordsets:new()};

new({_Scheme, _Term}=Id) ->
    #collection{id=Id, elements=ordsets:new()}.


%% @doc Creates a new bounded collection
%% @end
-spec new(occi_category:id(), [occi_entity:t() | occi_entity:id()]) -> t().
new(Id, Elements) ->
    elements(Elements, new(Id)).


%% @doc Return collection id
%% @end
-spec id(t()) -> id().
id(#collection{id=Id}) ->
    Id.


%% @doc Get all entity ids
%% @end
-spec ids(t()) -> [].
ids(#collection{ elements=Elements }) ->
    lists:reverse(ordsets:fold(fun ({Id, _}, Acc) ->
				       [ Id | Acc ]
			       end, [], Elements)).


%% @doc Get all elements
%% @end
-spec elements(t()) -> ordsets:ordset().
elements(#collection{ elements=Elements }) ->
    Elements.


%% @doc Set elements or entities
%% @end
-spec elements([elem() | occi_entity:id() | occi_entity:t()], t()) -> t().
elements(Elements, #collection{}=C) ->
    Elements2 = lists:reverse(ordsets:fold(fun to_elem/2, ordsets:new(), Elements)),
    C#collection{ elements=Elements2 }.


%% @doc Append elements to the collection
%% @end
-spec append(ordsets:ordset(), t()) -> t().
append(NewElements, #collection{ elements=Elements }=C) ->
    Elements2 = ordsets:union(ordsets:fold(fun to_elem/2, ordsets:new(), NewElements), Elements),
    C#collection{ elements=Elements2 }.


%% @doc Collection size
%% @end
-spec size(t()) -> integer().
size(#collection{ elements=Elements }) ->
    ordsets:size(Elements).


%% @doc Load collection from iolist
%% @end
-spec load(occi_utils:mimetype(), iolist(), occi_ctx:t()) -> t().
load(Mimetype, Bin, Ctx) ->
    occi_rendering:load_collection(Mimetype, Bin, Ctx).


-spec from_map(occi_rendering:ast()) -> t().
from_map(Map) ->
    from_map2(new(), Map).


%% @doc Build collecton from AST
%% @end
-spec from_map(occi_category:id() | binary(), occi_rendering:ast()) -> t().
from_map(Id, Map) ->
    from_map2(new(Id), Map).


%% @doc Render collection into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), occi_ctx:t()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).

%%%
%%% Priv
%%%
from_map2(Coll, Map) ->
    Fun = fun (Id, Acc) when is_binary(Id) ->
		  [ Id | Acc ];
	      (Map1, Acc) when is_map(Map1) ->
		  case maps:is_key(kind, Map1) of
		      true ->
			  [ occi_entity:from_map(Map1) | Acc ];
		      false ->
			  [ maps:get(location, Map1, maps:get(id, Map1)) | Acc ]
		  end
	  end,
    E0 = lists:foldl(Fun, [], maps:get(entities, Map, [])),
    E1 = lists:foldl(Fun, E0, maps:get(resources, Map, [])),
    E2 = lists:foldl(Fun, E1, maps:get(links, Map, [])),
    append(E2, Coll).


to_elem(Location, Acc) when is_binary(Location) ->
    ordsets:add_element({Location, undefined}, Acc);

to_elem({Location, _}=E, Acc) when is_binary(Location) ->
    ordsets:add_element(E, Acc);

to_elem(E, Acc) when ?is_entity(E) ->
    Location = case occi_entity:location(E) of
		   undefined ->
		       throw({invalid_location, undefined});
		   L -> 
		       L
	       end,
    ordsets:add_element({Location, E}, Acc).
