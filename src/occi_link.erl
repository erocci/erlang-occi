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

-mixin([{occi_entity, except, [new/1, from_map/2, change_prefix/3]},
	occi_type]).

-export([new/1,
	 new/3,
	 new/5,
	 source/1,
	 target/1,
	 endpoint/2]).

-export([from_map/2, 
	 change_prefix/3]).

-opaque t() :: occi_entity:t().

-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


new(Kind) ->
    occi_entity:merge_parents(Kind, {link, undefined, occi_kind:id(Kind), [], #{}, #{}, #{}}).


%% @equiv new(KindId, Src, Target, occi_resource:kind(Target))
%% @end
-spec new(occi_category:id() | binary(), 
	  binary() | occi_resource:t(), 
	  binary() | occi_resource:t()) -> t().
new(KindId, Src, Target) when element(?class, Src) =:= resource, 
			      element(?class, Target) =:= resource ->
    TargetKind = case is_binary(Target) of
		     true -> undefined;
		     false -> occi_resource:kind(Target)
		 end,
    new(KindId, Src, occi_resource:kind(Src), Target, TargetKind).


%% @doc Creates a new link
%% @end
-spec new(occi_category:id() | binary(), 
	  binary(), 
	  occi_category:id(),
	  binary(),
	  occi_category:id() | undefined) -> t().
new(KindId, Src, SrcKind, Target, TargetKind) when is_binary(KindId) ->
    new(occi_category:parse_id(KindId), Src, SrcKind, Target, TargetKind);

new({_Scheme, _Term}=KindId, Src, SrcKind, Target, TargetKind) ->
    new(occi_models:kind(link, KindId), Src, SrcKind, Target, TargetKind);

new(Kind, Src, SrcKind, Target, TargetKind) ->
    Link = occi_entity:merge_parents(Kind, {link, undefined, occi_kind:id(Kind), [], #{}, #{}, #{}}),
    set(#{ <<"occi.core.source">> => Src, 
	   <<"occi.core.source.kind">> => SrcKind,
	   <<"occi.core.target">> => Target,
	   <<"occi.core.target.kind">> => TargetKind }, internal, Link).


-spec source(t()) -> occi_uri:url().
source(E) ->
    get(<<"occi.core.source">>, E).


-spec target(t()) -> occi_uri:url().
target(E) ->
    get(<<"occi.core.target">>, E).


-spec from_map(occi_kind:t(), occi_rendering:ast()) -> t().
from_map(Kind, Map) ->
    try begin
	    Src = maps:get(source, Map),
	    Target = maps:get(target, Map),
	    L = new(Kind,
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


%% @doc Make source / target urls relative to endpoint
%% URL are canonicalized: default ports are added to scheme if necessary
%% Throws `{invalid_link, binary()}' if source is outside of endpoint's domain
%% @throws {invalid_link, binary()}
%% @end
-spec endpoint(occi_uri:url(), t()) -> t().
endpoint(Endpoint, Link) ->
    Source = case source(Link) of
		 undefined ->
		     undefined;
		 Source0 ->
		     case occi_uri:relative(Endpoint, Source0) of
			 out_of_domain -> throw({invalid_link, Source0});
			 S -> S
		     end
	     end,
    Target0 = target(Link),
    Target = case occi_uri:relative(Endpoint, Target0) of
		 out_of_domain -> Target0;
		 T -> T
	     end,
    occi_entity:set(#{ <<"occi.core.source">> => Source,
		       <<"occi.core.target">> => Target }, internal, Link).


%% @doc Change urls prefix
%% @end
-spec change_prefix(occi_uri:prefix_op(), binary(), t()) -> t().
change_prefix(Op, Prefix, Link) ->
    Link2 = case element(?location, Link) of
		undefined ->
		    Link;
		Location ->
		    Location2 = occi_uri:change_prefix(Op, Prefix, Location),
		    location(Location2, Link)
	    end,
    Source = occi_uri:change_prefix(Op, Prefix, occi_entity:get(<<"occi.core.source">>, Link2)),
    Target = occi_uri:change_prefix(Op, Prefix, occi_entity:get(<<"occi.core.target">>, Link2)),
    occi_entity:update(#{ <<"occi.core.source">> => Source,
			  <<"occi.core.target">> => Target }, internal, Link2).


%%%
%%% Priv
%%%
