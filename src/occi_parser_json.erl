%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_json).

-include("occi_rendering.hrl").
-include("occi_log.hrl").
-include("occi_entity.hrl").
-include_lib("annotations/include/annotations.hrl").

-export([parse_model/3,
	 parse_entity/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec parse_model(extension | kind | mixin | action, binary(), parse_ctx()) -> occi_type:t().
parse_model(mixin, Bin, Ctx) ->
    Map = jsx:decode(Bin, [return_maps]),
    case maps:get(<<"mixins">>, Map, []) of
	[] ->
	    throw({parse_error, {mixin, no_mixin}});
	[Mixin] ->
	    p_mixin(Mixin, Ctx);
	_Else ->
	    throw({parse_error, {mixin, multiple_mixin}})
    end;

parse_model(Type, _Bin, _Ctx) when Type =:= extension;
			     Type =:= kind;
			     Type =:= action ->
    throw({not_implemented, Type}).


-spec parse_entity(entity | resource | link, binary(), parse_ctx()) -> occi_type:t().
parse_entity(Type, Bin, Ctx) when Type =:= entity;
				  Type =:= resource;
				  Type =:= link ->
    Entity = p_entity(jsx:decode(Bin, [return_maps]), Ctx),
    case occi_entity:is_subtype(Type, Entity) of
	true -> Entity;
	false -> throw({parse_error, {type, Entity}})
    end.


%%%
%%% Parsers
%%%
p_mixin(#{ <<"scheme">> := Scheme }=Map, Ctx) ->
    p_mixin2(Map, binary_to_list(Scheme), Ctx);

p_mixin(_, _) ->
    throw({parse_error, {mixin, missing_scheme}}).


p_mixin2(#{ <<"term">> := Term}=Map, Scheme, Ctx) ->
    p_mixin3(Map, Scheme, binary_to_list(Term), Ctx);

p_mixin2(_, _, _) ->
    throw({parse_error, {mixin, missing_term}}).


p_mixin3(#{ <<"location">> := Location}=Map, Scheme, Term, Ctx) ->
    M0 = occi_mixin:new(Scheme, Term),
    AbsLoc = occi_uri:to_abs(Location, Ctx#parse_ctx.url),
    M1 = occi_mixin:location(AbsLoc, M0),
    case maps:get(<<"title">>, Map, undefined) of
	undefined ->
	    M1;
	Title ->
	    occi_mixin:title(binary_to_list(Title), M1)
    end;

p_mixin3(_, _, _, _) ->
    throw({parse_error, {mixin, missing_location}}).


p_entity(#{ <<"id">> := Id }=Map, Ctx) ->
    Url = occi_uri:to_abs(Id, Ctx#parse_ctx.url),
    p_entity2(Url, Map, Ctx);
	
p_entity(_Map, _Ctx) ->
    throw({parse_error, missing_id}).


p_entity2(Id, #{ <<"kind">> := KindId }=Map, Ctx) ->
    Mixins = lists:foldr(fun (MixinId, Acc) ->
				 [ occi_category:parse_id(MixinId) | Acc ]
			 end, [], maps:get(<<"mixins">>, Map, [])),
    Kind = occi_models:category(KindId),
    case occi_kind:has_parent(resource, Kind) of
	true ->
	    p_resource(Id, Kind, Mixins, Map, Ctx);
	false ->
	    p_link(Id, Kind, Mixins, Map, Ctx)
    end;

p_entity2(_Id, _Map, _Ctx) ->
    throw({parse_error, missing_kind}).


p_resource(Id, Kind, Mixins, Map, Ctx) ->
    R = occi_resource:new(Id, Kind),
    R1 = lists:foldl(fun (MixinId, Acc) ->
			     occi_resource:add_mixin(MixinId, Acc)
		     end, R, Mixins),
    Attributes = maps:merge(
		   maps:fold(fun (K, V, Acc) ->
				     Acc#{ binary_to_list(K) => V }
			     end, #{}, maps:get(<<"attributes">>, Map, #{})),
		   #{ "occi.core.summary" => maps:get(<<"summary">>, Map, <<>>),
		      "occi.core.title" => maps:get(<<"title">>, Map, <<>>) }),
    R2 = occi_entity:set(Attributes, Ctx#parse_ctx.valid, R1),
    p_resource_links(maps:get(<<"links">>, Map, []), Ctx, R2).


p_resource_links([], _Valid, R) ->
    R;

p_resource_links([ Link | Tail ], Valid, R) ->
    Source = #{ <<"location">> => occi_uri:to_string(occi_resource:id(R)), 
		<<"kind">> => occi_resource:kind(R) },
    L = p_resource_link(Link#{ <<"source">> => Source }, Valid),
    p_resource_links(Tail, Valid, occi_resource:add_link(L, R)).


p_resource_link(#{ <<"id">> := Id }=Link, Ctx) ->
    Url = occi_uri:to_abs(Id, Ctx#parse_ctx.url),
    p_resource_link2(Url, Link, Ctx);

p_resource_link(_, _) ->
    throw({parse_error, missing_id}).


p_resource_link2(Id, #{ <<"kind">> := Kind }=Link, Ctx) ->
    p_link(Id, Kind, maps:get(<<"mixins">>, Link, []), Link, Ctx);

p_resource_link2(_, _, _) ->
    throw({parse_error, missing_kind}).


p_link(Id, Kind, Mixins,
       #{ <<"source">> := #{ <<"location">> := SourceLocation }=Source }=Map, Ctx) ->
    p_link2(Id, Kind, Mixins, 
	    occi_uri:to_abs(SourceLocation, Ctx#parse_ctx.url), Source, Map, Ctx);

p_link(_Id, _Kind, _Mixins, _Map, _Ctx) ->
    throw({parse_error, {link, missing_source}}).


p_link2(Id, Kind, Mixins, SourceLocation, Source, 
	#{ <<"target">> := #{ <<"location">> := TargetLocation }=Target }=Map, Ctx) ->
    SourceKind = maps:get(<<"kind">>, Source, undefined),
    TargetKind = maps:get(<<"kind">>, Target, undefined),
    Link = occi_link:new(Id, Kind,
			 SourceLocation, SourceKind, 
			 occi_uri:to_abs(TargetLocation, Ctx#parse_ctx.url), TargetKind),
    Link1 = lists:foldl(fun (MixinId, Acc) ->
				occi_resource:add_mixin(MixinId, Acc)
			end, Link, Mixins),
    Attributes = maps:merge(
		   maps:fold(fun (K, V, Acc) ->
				     Acc#{ binary_to_list(K) => V }
			     end, #{}, maps:get(<<"attributes">>, Map, #{})),
		   #{ "occi.core.title" => maps:get(<<"title">>, Map, <<>>) }),
    occi_link:set(Attributes, Ctx#parse_ctx.valid, Link1);

p_link2(_, _, _, _, _, _, _) ->
    throw({parse_error, {link, missing_target}}).


%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
