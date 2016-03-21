%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
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

-export([load/3]).

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


%% @doc Load action from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist(), parse_ctx()) -> t().
load(Mimetype, Bin, Ctx) -> 
    occi_rendering:load_model(action, Mimetype, Bin, Ctx).
