%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_mixin).

-include("occi_category.hrl").

-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2]}]).

-export([new/2,
	 add_apply/2,
	 applies/1,
	 add_depend/2,
	 depends/1]).

-export([load/3]).

-mixin([occi_type]).

-record(mixin, {id :: occi_category:id(), m :: #{}}).
-type t() :: #mixin{}.

-export_type([t/0]).

-spec new(Scheme :: binary(), Term :: binary()) -> t().
new(Scheme, Term) ->
    M0 = occi_category:new(Scheme, Term, mixin),
    Map = M0#mixin.m,
    M0#mixin{m = Map#{applies => [], depends => [], actions => #{}, location => undefined} }.


-spec add_apply(binary() | occi_category:id(), t()) -> t().
add_apply(Apply, #{ applies := Applies }=Mixin) when is_binary(Apply) ->
    ApplyId = occi_category:parse_id(Apply),
    ?s(applies, [ ApplyId | Applies ], Mixin);

add_apply({_Scheme, _Term}=Apply, Mixin) ->
    Applies = ?g(applies, Mixin),
    ?s(applies, [ Apply | Applies ], Mixin).


-spec applies(t()) -> [occi_category:id()].
applies(M) ->
    ?g(applies, M).


-spec add_depend(binary() | occi_category:id(), t()) -> t().
add_depend(Depend, Mixin) when is_binary(Depend) ->
    DependId = occi_category:parse_id(Depend),
    Depends = ?g(depends, Mixin),
    ?s(depends, [ DependId | Depends ], Mixin);

add_depend({_Scheme, _Term}=Depend, Mixin) ->
    Depends = ?g(depends, Mixin),
    ?s(depends, [ Depend | Depends ], Mixin).


-spec depends(t()) -> [occi_category:id()].
depends(M) ->
    ?g(depends, M).


%% @doc Load mixin from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist(), parse_ctx()) -> t().
load(Mimetype, Bin, Ctx) -> 
    occi_rendering:load_model(mixin, Mimetype, Bin, Ctx).
