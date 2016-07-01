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
	 entity/2,
	 locations/1,
	 elements/1,
	 elements/2,
	 size/1,
	 delete/2,
	 append/2,
	 endpoint/2]).

-export([from_map/1,
	 from_map/2,
	 render/3]).

-type id() :: binary() | occi_category:id().
-type elem() :: {occi_uri:url(), occi_entity:t() | undefined}.

-record(collection, {id                    :: id(),
		     elements = #{}        :: maps:map()}).

-opaque t() :: #collection{}.

-export_type([t/0]).


%% @doc Create a new collection
%% @end
-spec new() -> t().
new() ->
    #collection{id=undefined, elements=#{}}.


%% @doc Create a new collection.
%% If id is an uri, collection is unbounded.
%% If id is a category id, collection is bounded
%% @end
-spec new(binary() | occi_category:id()) -> t().
new(Id) when is_binary(Id) ->
    #collection{id=Id, elements=#{}};

new({_Scheme, _Term}=Id) ->
    #collection{id=Id, elements=#{}}.


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


%% @doc Get an entity given a location
%% @end
-spec entity(occi_uri:url(), t()) -> occi_entity:t() | undefined.
entity(Location, #collection{ elements=Elements }) ->
    maps:get(Location, Elements, undefined).


%% @doc Get all entity locations
%% @end
-spec locations(t()) -> [].
locations(#collection{ elements=Elements }) ->
    maps:keys(Elements).


%% @doc Get all elements
%% @end
-spec elements(t()) -> [elem()].
elements(#collection{ elements=Elements }) ->
    maps:to_list(Elements).


%% @doc Set elements or entities
%% @end
-spec elements([elem() | occi_entity:id() | occi_entity:t()], t()) -> t().
elements(Elements, #collection{}=C) ->
    Elements2 = lists:foldl(fun to_elem/2, #{}, Elements),
    C#collection{ elements=Elements2 }.


%% @doc Append elements to the collection
%% If element is already present, it is replaced
%% @end
-spec append([elem()] | occi_entity:t(), t()) -> t().
append(Element, Coll) when ?is_entity(Element) ->
    append([Element], Coll);

append(NewElements, #collection{ elements=Elements }=C) ->
    Elements2 = lists:foldl(fun to_elem/2, Elements, NewElements),
    C#collection{ elements=Elements2 }.


%% @doc Delete elements from collection
%% @end
-spec delete([occi_uri:url()] | occi_uri:url() | occi_entity:t(), t()) -> t().
delete(ToDelete, #collection{ elements=Elements }=C) when is_list(ToDelete) ->
    Elements2 = lists:foldl(fun (Entity, Acc) when ?is_entity(Entity) ->
				    maps:remove(occi_entity:location(Entity), Acc);
				(Location, Acc) when is_binary(Location) ->
				    maps:remove(Location, Acc)
			    end, Elements, ToDelete),
    C#collection{ elements=Elements2 };

delete(ToDelete, Coll) ->
    delete([ToDelete], Coll).
				   

%% @doc Collection size
%% @end
-spec size(t()) -> integer().
size(#collection{ elements=Elements }) ->
    maps:size(Elements).


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
-spec render(occi_utils:mimetype(), t(), occi_uri:t()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).


%% @doc Remove endpoint from locations if necessary
%% @end
-spec endpoint(occi_uri:url(), t()) -> t().
endpoint(Endpoint, #collection{ elements=Elements }=Coll) ->
    Elements2 = maps:fold(fun (Location, Entity, Acc) ->
				  Acc#{ occi_uri:relative(Endpoint, Location) => Entity }
			  end, #{}, Elements),
    Coll#collection{ elements=Elements2 }.


%%%
%%% Priv
%%%
from_map2(Coll, Map) ->
    Fun = fun (Location, Acc) when is_binary(Location) ->
		  [ Location | Acc ];
	      (Map1, Acc) when is_map(Map1) ->
		  case maps:is_key(kind, Map1) of
		      true ->
			  [ occi_entity:from_map(Map1) | Acc ];
		      false ->
			  Location = maps:get(location, Map1),
			  [ Location | Acc ]
		  end
	  end,
    E0 = lists:foldl(Fun, [], maps:get(entities, Map, [])),
    E1 = lists:foldl(Fun, E0, maps:get(resources, Map, [])),
    E2 = lists:foldl(Fun, E1, maps:get(links, Map, [])),
    append(E2, Coll).


to_elem(Location, Acc) when is_binary(Location) ->
    Acc#{ Location => undefined };

to_elem({Location, Entity}, Acc) when is_binary(Location) ->
    Acc#{ Location => Entity };

to_elem(E, Acc) when ?is_entity(E) ->
    Location = case occi_entity:location(E) of
		   undefined ->
		       throw({invalid_location, undefined});
		   L -> 
		       L
	       end,
    Acc#{ Location => E }.
