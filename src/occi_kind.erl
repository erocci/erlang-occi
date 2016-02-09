%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_kind).

-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2]}]).

-export([new/2,
	 parent/1,
	 parent/2]).

-type t() :: #{}.

-export_type([t/0]).

-spec new(Scheme :: string(), Term :: string()) -> t().
new(Scheme, Term) ->
    Kind = occi_category:new(Scheme, Term, kind),
    Kind#{ parent => undefined, actions => #{} }.


-spec parent(t()) -> occi_category:id().
parent(Kind) ->
    maps:get(parent, Kind).


-spec parent(string() | binary() | occi_category:id(), t()) -> t().
parent(Parent, Kind) when is_list(Parent); is_binary(Parent) ->
    ParentId = occi_category:parse_id(Parent),
    Kind#{ parent := ParentId };

parent({_Scheme, _Term}=ParentId, Kind) ->
    Kind#{ parent := ParentId }.
