%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_kind).

-include("occi.hrl").
-include("occi_category.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2]}]).

-export([new/2,
	 parent/1,
	 parent/2,
	 parents/1,
	 parents/2,
	 has_parent/2,
	 known_parent/1]).

-export([from_map/1]).

-mixin([occi_type]).


-record(kind, {id :: occi_category:id(), m :: #{} }).
-type t() :: #kind{}.

-export_type([t/0]).

-spec new(Scheme :: binary(), Term :: binary()) -> t().
new(Scheme, Term) ->
    K0 = occi_category:new(Scheme, Term, kind),
    M = K0#kind.m,
    M1 = M#{parents => [], actions => #{}, location => undefined },
    K0#kind{m=M1}.


%% @doc Return parent of the category
%% @end
-spec parent(t()) -> occi_category:id() | undefined.
parent(Kind) ->
    case ?g(parents, Kind) of
	[] -> undefined;
	[Parent | _] -> Parent
    end.


%% @doc Set parent of the category
%% @end
-spec parent(binary() | occi_category:id(), t()) -> t().
parent(Parent, Kind) when is_binary(Parent) ->
    ParentId = occi_category:parse_id(Parent),
    ?s(parents, [ ParentId | ?g(parents, Kind) ], Kind);

parent({_Scheme, _Term}=ParentId, Kind) ->
    ?s(parents, [ ParentId | ?g(parents, Kind) ], Kind).


%% @doc Return all ancestors
%% @end
-spec parents(t()) -> [occi_category:id()].
parents(Kind) ->
    ?g(parents, Kind).

%% @doc Set full list of parents
%% @end
-spec parents([occi_category:id()], t()) -> t().
parents(Parents, Kind) ->
    ?s(parents, Parents, Kind).


%% @doc Return true if kind has the parent
%% @end
-spec has_parent(occi_category:id() | resource | link, t()) -> boolean().
has_parent(resource, Kind) ->
    has_parent(?resource_kind_id, Kind);

has_parent(link, Kind) ->
    has_parent(?link_kind_id, Kind);

has_parent(Parent, Kind) ->
    has_parents2([ id(Kind) | parents(Kind) ], Parent).


%% @doc Return first known parent
%% @end
-spec known_parent(t()) -> resource | entity.
known_parent(Kind) ->
    known_parent2([ id(Kind) | parents(Kind) ]).


%% @doc Build kind from AST
%% @end
-spec from_map(occi_rendering:ast()) -> t().
from_map(Map) -> 
    try begin
	    Scheme = maps:get(scheme, Map),
	    Term = maps:get(term, Map),
	    K = new(Scheme, Term),
	    K1 = case maps:get(title, Map, undefined) of
		     undefined -> K;
		     Title -> title(Title, K)
		 end,
	    K2 = maps:fold(fun (Name, Spec, Acc1) ->
				   add_attribute(occi_attribute:from_map(Name, {Scheme, Term}, Spec), Acc1)
			   end, K1, maps:get(attributes, Map, [])),
	    K3 = lists:foldl(fun (Map2, Acc2) ->
				     add_action(occi_action:from_map({Scheme, Term}, Map2), Acc2)
			     end, K2, maps:get(actions, Map, [])),
	    case maps:get(parent, Map, undefined) of
		undefined -> K3;
		Parent -> parent(Parent, K3)
	    end
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.


%%%
%%% Priv
%%%
has_parents2([], _) ->
    false;

has_parents2([ Parent | _Tail ], Parent) ->
    true;

has_parents2([ _ | Tail ], Parent) ->
    has_parents2(Tail, Parent).


known_parent2([]) ->
    throw(unknown_parent);

known_parent2([ ?resource_kind_id | _ ]) ->
    resource;

known_parent2([ ?link_kind_id | _ ]) ->
    link;

known_parent2([ _ | Tail ]) ->
    known_parent2(Tail).
