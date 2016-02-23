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
	 parent/2]).


-record(kind, {id :: occi_category:id(), m :: #{} }).
-type t() :: #kind{}.

-export_type([t/0]).

-spec new(Scheme :: string(), Term :: string()) -> t().
new(Scheme, Term) ->
    K0 = occi_category:new(Scheme, Term, kind),
    M = K0#kind.m,
    M1 = M#{parent => undefined, actions => #{}, location => undefined },
    K0#kind{m=M1}.


-spec parent(t()) -> occi_category:id().
parent(Kind) ->
    ?g(parent, Kind).


-spec parent(string() | binary() | occi_category:id(), t()) -> t().
parent(Parent, Kind) when is_list(Parent); is_binary(Parent) ->
    ParentId = occi_category:parse_id(Parent),
    ?s(parent, ParentId, Kind);

parent({_Scheme, _Term}=ParentId, Kind) ->
    ?s(parent, ParentId, Kind).
