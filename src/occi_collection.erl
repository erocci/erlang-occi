%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 24 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_collection).

-include("occi_uri.hrl").
-include("occi_type.hrl").

-export([new/0,
	 new/1,
	 new/2,
	 id/1,
	 ids/1,
	 elements/1,
	 elements/2,
	 size/1,
	 append/2]).

-export([from_map/2,
	 load/3,
	 render/3]).

-type id() :: binary() | occi_category:id().
-type elem() :: {occi_entity:id(), occi_entity:t() | undefined}.

-record(collection, {id                    :: id(),
		     elements = sets:new() :: sets:set()}).

-type t() :: #collection{}.


%% @doc Create a new collection
%% @end
-spec new() -> t().
new() ->
    #collection{id=undefined, elements=sets:new()}.


%% @doc Create a new collection.
%% If id is an uri, collection is unbounded.
%% If id is a category id, collection is bounded
%% @end
-spec new(binary() | occi_category:id()) -> t().
new(Id) when is_binary(Id) ->
    #collection{id=Id};

new({_Scheme, _Term}=Id) ->
    #collection{id=Id}.


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
-spec ids(t()) -> sets:set().
ids(#collection{ elements=Elements }) ->
    sets:fold(fun ({Id, _}, Acc) ->
		      sets:add_element(Id, Acc)
	      end, sets:new(), Elements).


%% @doc Get all elements
%% @end
-spec elements(t()) -> sets:set().
elements(#collection{ elements=Elements }) ->
    Elements.


%% @doc Set elements or entities
%% @end
-spec elements([elem() | occi_entity:id() | occi_entity:t()], t()) -> t().
elements(Elements, #collection{}=C) ->
    Elements2 = sets:fold(fun to_elem/2, sets:new(), Elements),
    C#collection{ elements=Elements2 }.


%% @doc Append elements to the collection
%% @end
-spec append([elem()] | sets:set(), t()) -> t().
append(NewElements, Coll) when is_list(NewElements) ->
    append(sets:from_list(NewElements), Coll);

append(NewElements, #collection{ elements=Elements }=C) ->
    Elements2 = sets:union(sets:fold(fun to_elem/2, sets:new(), NewElements), Elements),
    C#collection{ elements=Elements2 }.


%% @doc Collection size
%% @end
-spec size(t()) -> integer().
size(#collection{ elements=Elements }) ->
    sets:size(Elements).


%% @doc Load collection from iolist
%% @end
-spec load(occi_utils:mimetype(), iolist(), occi_ctx:t()) -> t().
load(Mimetype, Bin, Ctx) ->
    occi_rendering:load_collection(Mimetype, Bin, Ctx).

%% @doc Build collecton from AST
%% @end
-spec from_map(occi_category:id() | binary(), occi_rendering:ast()) -> t().
from_map(Id, Map) ->
    C = new(Id),
    Elements = maps:get(entities, Map, []) 
	++ maps:get(resources, Map, [])
	++ maps:get(links, Map, []),
    append(Elements, C).


%% @doc Render collection into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), occi_ctx:t()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).

%%%
%%% Priv
%%%
to_elem(E, Acc) when is_binary(E) ->
    sets:add_element(E, Acc);

to_elem({Id, _}=E, Acc) when is_binary(Id) ->
    sets:add_element(E, Acc);

to_elem(E, Acc) when ?is_entity(E) ->
    sets:add_element(E, Acc).
