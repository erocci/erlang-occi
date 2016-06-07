%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_link).

-include("occi.hrl").
-include("occi_uri.hrl").
-include("occi_entity.hrl").
-include("occi_log.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [from_map/2, change_prefix/3]},
	occi_type]).

-export([new/4,
	 new/6,
	 source/1,
	 target/1]).

-export([from_map/2, 
	 change_prefix/3]).

-opaque t() :: occi_entity:t().

-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @equiv new(Id, KindId, Src, Target, occi_resource:kind(Target))
%% @end
-spec new(uri:t(), occi_category:id() | binary(), 
	  binary() | occi_resource:t(), 
	  binary() | occi_resource:t()) -> t().
new(Id, KindId, Src, Target) when ?is_uri(Id), 
				  element(?class, Src) =:= resource, 
				  element(?class, Target) =:= resource ->
    TargetKind = case is_binary(Target) of
		     true -> undefined;
		     false -> occi_resource:kind(Target)
		 end,
    new(Id, KindId, Src, occi_resource:kind(Src), Target, TargetKind).


%% @doc Creates a new link
%% @end
-spec new(occi_uri:url() | undefined, occi_category:id() | binary(), 
	  binary(), 
	  occi_category:id(),
	  binary(),
	  occi_category:id() | undefined) -> t().
new(Id, KindId, Src, SrcKind, Target, TargetKind) when is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId), Src, SrcKind, Target, TargetKind);

new(Id, {_Scheme, _Term}=KindId, Src, SrcKind, Target, TargetKind) ->
    new(Id, occi_models:kind(link, KindId), Src, SrcKind, Target, TargetKind);

new(Id, Kind, Src, SrcKind, Target, TargetKind) ->
    Link = occi_entity:merge_parents(Kind, {link, Id, undefined, occi_kind:id(Kind), [], #{}, #{}, #{}}),
    set(#{ <<"occi.core.source">> => Src, 
	   <<"occi.core.source.kind">> => SrcKind,
	   <<"occi.core.target">> => Target,
	   <<"occi.core.target.kind">> => TargetKind }, internal, Link).


-spec source(t()) -> occi_uri:t().
source(E) ->
    get(<<"occi.core.source">>, E).


-spec target(t()) -> occi_uri:t().
target(E) ->
    get(<<"occi.core.target">>, E).


-spec from_map(occi_kind:t(), occi_rendering:ast()) -> t().
from_map(Kind, Map) ->
    try begin
	    Id = maps:get(id, Map, undefined),
	    Src = maps:get(source, Map),
	    Target = maps:get(target, Map),
	    L = new(Id, Kind,
		    maps:get(location, Src), maps:get(kind, Src, undefined), 
		    maps:get(location, Target), maps:get(kind, Target, undefined)),
	    L0 = case maps:get(location, Map, undefined) of
		     undefined -> L;
		     Location -> occi_entity:location(Location, L)
		 end,
	    L1 = lists:foldl(fun (M, Acc1) ->
				     add_mixin(M, Acc1)
			     end, L0, maps:get(mixins, Map, [])),
	    Attrs0 = maps:get(attributes, Map, #{}),
	    occi_entity:set(Attrs0, client, L1)
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.


%% @doc Change urls prefix
%% @end
-spec change_prefix(occi_uri:prefix_op(), binary(), t()) -> t().
change_prefix(Op, Prefix, Link) ->
    Location2 = occi_uri:change_prefix(Op, Prefix, element(?location, Link)),
    Link2 = location(Location2, Link),
    Source = occi_uri:change_prefix(Op, Prefix, occi_entity:get(<<"occi.core.source">>, Link2)),
    Target = occi_uri:change_prefix(Op, Prefix, occi_entity:get(<<"occi.core.target">>, Link2)),
    occi_entity:update(#{ <<"occi.core.source">> => Source,
			  <<"occi.core.target">> => Target }, internal, Link2).
