%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 24 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_collection).

-include("occi_uri.hrl").
-include("occi_rendering.hrl").

-export([new/0,
	 new/1,
	 id/1,
	 ids/1,
	 elements/1,
	 elements/2,
	 append/2]).

-export([load/3,
	 render/3]).

-type id() :: occi_uri:t() | occi_category:id().
-type elem() :: {occi_entity:id(), occi_entity:t() | undefined}.

-record(collection, {id            :: id(),
		     elements = [] :: [elem()]}).

-type t() :: #collection{}.


%% @doc Create a new collection
%% @end
-spec new() -> t().
new() ->
    #collection{id=undefined}.


%% @doc Create a new collection.
%% If id is an uri, collection is unbounded.
%% If id is a category id, collection is bounded
%% @end
-spec new(occi_uri:t() | occi_category:id()) -> t().
new(Id) when ?is_uri(Id) ->
    #collection{id=Id};

new({_Scheme, _Term}=Id) ->
    #collection{id=Id}.


%% @doc Return collection id
%% @end
-spec id(t()) -> id().
id(#collection{id=Id}) ->
    Id.


%% @doc Get all entity ids
%% @end
-spec ids(t()) -> [occi_entity:id()].
ids(#collection{ elements=Elements }) ->
    lists:map(fun ({Id, _}) ->
		      Id
	      end, Elements).


%% @doc Get all elements
%% @end
-spec elements(t()) -> [elem()].
elements(#collection{ elements=Elements }) ->
    Elements.


%% @doc Set elements or entities
%% @end
-spec elements([elem() | occi_entity:id() | occi_entity:t()], t()) -> t().
elements(Elements, #collection{}=C) ->
    L = lists:map(fun to_elem/1, Elements),
    C#collection{ elements=L }.


%% @doc Append elements to the collection
%% @end
-spec append([elem()], t()) -> t().
append(NewElements, #collection{ elements=Elements }=C) ->
    C#collection{ elements=lists:map(fun to_elem/1, NewElements) ++ Elements }.


%% @doc Load collection from iolist
%% @end
-spec load(occi_utils:mimetype(), iolist(), parse_ctx()) -> t().
load(Mimetype, Bin, Ctx) ->
    occi_rendering:load_collection(Mimetype, Bin, Ctx).


%% @doc Render collection into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), render_ctx()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).

%%%
%%% Priv
%%%
to_elem(E) when ?is_uri(E) ->
    {E, undefined};

to_elem({Id, _}=E) when ?is_uri(Id) ->
    E;

to_elem(E) when element(1, E) =:= resource;
		element(1, E) =:= link ->
    {occi_entity:id(E), E}.
