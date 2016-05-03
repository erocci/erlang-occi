%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Implements action definition (category)
%%% For action invocation, @see occi_invoke
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_action).

-include("occi_category.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2, new/3]},
	occi_type]).

-export([new/3,
	 category/1]).

-export([from_map/2]).

-record(action, { id :: occi_category:id(), m :: #{} }).
-type t() :: #action{}.

-export_type([t/0]).


%% @doc Create new action category
%% @end
-spec new(Scheme :: string(), Term :: string(), Related :: occi_category:t()) -> t().
new(Scheme, Term, Related) ->
    A = occi_category:new(Scheme, Term, action),
    Map = A#action.m,
    A#action{ m = Map#{ category => Related } }.


%% @doc Get related category (kind or mixin)
%% @end
-spec category(t()) -> occi_category:id().
category(A) ->
    ?g(category, A).


%% @doc Load action from ast
%% @end
-spec from_map(occi_category:id(), occi_rendering:ast()) -> t().
from_map(Related, Map) -> 
    try begin
	    Scheme = maps:get(scheme, Map),
	    Term = maps:get(term, Map),
	    A = new(Scheme, Term, Related),
	    A0 = case maps:get(title, Map, undefined) of
		     undefined -> A;
		     Title -> title(Title, A)
		 end,
	    maps:fold(fun (Name, Spec, Acc1) ->
			      add_attribute(occi_attribute:from_map(Name, {Scheme, Term}, Spec), Acc1)
		      end, A0, maps:get(attributes, Map, #{}))
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.
