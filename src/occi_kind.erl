%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_kind).

-include("occi_category.hrl").
-include_lib("mixer/include/mixer.hrl").
-include_lib("annotations/include/annotations.hrl").

-mixin([{occi_category, except, [new/2]}]).

-export([new/2,
	 parent/1,
	 parent/2,
	 parents/1,
	 parents/2]).


-record(kind, {id :: occi_category:id(), m :: #{} }).
-type t() :: #kind{}.

-export_type([t/0]).

-spec new(Scheme :: string(), Term :: string()) -> t().
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
-spec parent(string() | binary() | occi_category:id(), t()) -> t().
parent(Parent, Kind) when is_list(Parent); is_binary(Parent) ->
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
