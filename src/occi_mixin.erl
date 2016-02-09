%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_mixin).

-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2]}]).

-export([new/2,
	 add_apply/2,
	 applies/1,
	 add_depend/2,
	 depends/1]).

-type t() :: #{}.

-export_type([t/0]).

-spec new(Scheme :: string(), Term :: string()) -> t().
new(Scheme, Term) ->
    Mixin = occi_category:new(Scheme, Term, mixin),
    Mixin#{ applies => [], depends => [], actions => #{} }.


-spec add_apply(string() | binary() | occi_category:id(), t()) -> t().
add_apply(Apply, #{ applies := Applies }=Mixin) when is_list(Apply); is_binary(Apply) ->
    ApplyId = occi_category:parse_id(Apply),
    Mixin#{ applies := [ ApplyId | Applies ] };

add_apply({_Scheme, _Term}=Apply, #{ applies := Applies }=Mixin) ->
    Mixin#{ applies := [ Apply | Applies ]}.


-spec applies(t()) -> [occi_category:id()].
applies(M) ->
    maps:get(applies, M).


-spec add_depend(string() | binary() | occi_category:id(), t()) -> t().
add_depend(Depend, #{ depends := Depends }=Mixin) when is_list(Depend); is_binary(Depend) ->
    DependId = occi_category:parse_id(Depend),
    Mixin#{ depends := [ DependId | Depends ] };

add_depend({_Scheme, _Term}=Depend, #{ depends := Depends }=Mixin) ->
    Mixin#{ depends := [ Depend | Depends ]}.


-spec depends(t()) -> [occi_category:id()].
depends(M) ->
    maps:get(depends, M).
