%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_link).

-include("occi.hrl").
-include("occi_entity.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [load/3]},
	occi_type]).

-export([new/4,
	 new/6,
	 source/1,
	 target/1]).

-export([load/3]).

-type t() :: occi_entity:t().
-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @equiv new(Id, KindId, Src, Target, occi_resource:kind(Target) | undefined)
%% @end
-spec new(string(), occi_category:id() | string() | binary(), 
	  string() | occi_resource:t(), 
	  string() | occi_resource:t()) -> t().
new(Id, KindId, Src, Target) when is_list(Id), 
				  element(?class, Src) =:= resource, 
				  element(?class, Target) =:= resource ->
    TargetKind = case is_list(Target) of
		     true -> undefined;
		     false -> occi_resource:kind(Target)
		 end,
    new(Id, KindId, Src, occi_resource:kind(Src), Target, TargetKind).


%% @doc Creates a new link
%% @end
-spec new(string(), occi_category:id() | string() | binary(), 
	  string(), 
	  occi_category:id(),
	  string(),
	  occi_category:id() | undefined) -> t().
new(Id, KindId, Src, SrcKind, Target, TargetKind) when is_list(KindId); is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId), Src, SrcKind, Target, TargetKind);

new(Id, {_Scheme, _Term}=KindId, Src, SrcKind, Target, TargetKind) ->
    new(Id, occi_models:kind(link, KindId), Src, SrcKind, Target, TargetKind);

new(Id, Kind, Src, SrcKind, Target, TargetKind) when is_list(Id), 
						     is_list(Src),
						     is_list(Target) ->
    Link = occi_entity:merge_parents(Kind, {link, Id, occi_kind:id(Kind), [], #{}, #{}}),
    set(#{ "occi.core.source" => Src, 
	   "occi.core.source.kind" => SrcKind,
	   "occi.core.target" => Target,
	   "occi.core.target.kind" => TargetKind }, internal, Link).


-spec source(t()) -> string().
source(E) ->
    get("occi.core.source", E).


-spec target(t()) -> string().
target(E) ->
    get("occi.core.target", E).


%% @doc Load link from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist(), occi_entity:validation()) -> t().
load(Mimetype, Bin, V) -> 
    occi_rendering:load_entity(link, Mimetype, Bin, V).
