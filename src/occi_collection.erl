%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 24 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_collection).

-include("occi_uri.hrl").

-export([new/1,
	 id/1,
	 entities/1,
	 entities/2,
	 append/2]).

-type id() :: occi_uri:t() | occi_category:id().

-record(collection, {id            :: id(),
		     entities = [] :: [occi_entity:t()]}).

-type t() :: #collection{}.


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


%% @doc Get all entities
%% @end
-spec entities(t()) -> [occi_entity:t()].
entities(#collection{ entities=E }) ->
    E.


%% @doc Set entities
%% @end
-spec entities([occi_entity:t()], t()) -> t().
entities(Entities, #collection{}=C) ->
     C#collection{ entities=Entities }.


%% @doc Append entities to the collection
%% @end
-spec append([occi_entity:t()], t()) -> t().
append(NewEntities, #collection{ entities=Entities }=C) ->
    C#collection{ entities=NewEntities ++ Entities }.
