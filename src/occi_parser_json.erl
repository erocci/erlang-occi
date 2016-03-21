%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_json).

-include("occi_log.hrl").
-include("occi_entity.hrl").
-include_lib("annotations/include/annotations.hrl").

-export([parse_model/2,
	 parse_entity/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec parse_model(extension | kind | mixin | action, binary()) -> occi_type:t().
parse_model(mixin, Bin) ->
    Map = jsx:decode(Bin, [return_maps]),
    case maps:get(<<"mixins">>, Map, []) of
	[] ->
	    throw({parse_error, {mixin, no_mixin}});
	[Mixin] ->
	    p_mixin(Mixin);
	_Else ->
	    throw({parse_error, {mixin, multiple_mixin}})
    end;

parse_model(Type, _Bin) when Type =:= extension;
			     Type =:= kind;
			     Type =:= action ->
    throw({non_implemented, Type}).


-spec parse_entity(entity | resource | link, binary(), occi_entity:validation()) -> occi_type:t().
parse_entity(Type, Bin, Valid) when Type =:= entity;
				    Type =:= resource;
				    Type =:= link ->
    Entity = p_entity(jsx:decode(Bin, [return_maps]), Valid),
    case occi_entity:is_subtype(Type, Entity) of
	true -> Entity;
	false -> throw({parse_error, {type, Entity}})
    end.


%%%
%%% Parsers
%%%
p_mixin(#{ <<"scheme">> := Scheme }=Map) ->
    p_mixin2(Map, binary_to_list(Scheme));

p_mixin(_) ->
    throw({parse_error, {mixin, missing_scheme}}).


p_mixin2(#{ <<"term">> := Term}=Map, Scheme) ->
    p_mixin3(Map, Scheme, binary_to_list(Term));

p_mixin2(_, _) ->
    throw({parse_error, {mixin, missing_term}}).


p_mixin3(#{ <<"location">> := Location}=Map, Scheme, Term) ->
    M0 = occi_mixin:new(Scheme, Term),
    M1 = occi_mixin:location(binary_to_list(Location), M0),
    case maps:get(<<"title">>, Map, undefined) of
	undefined ->
	    M1;
	Title ->
	    occi_mixin:title(binary_to_list(Title), M1)
    end;

p_mixin3(_, _, _) ->
    throw({parse_error, {mixin, missing_location}}).


p_entity(#{ <<"id">> := Id }=Map, Valid) ->
    p_entity2(binary_to_list(Id), Map, Valid);
	
p_entity(_Map, _Valid) ->
    throw({parse_error, missing_id}).


p_entity2(Id, #{ <<"kind">> := KindId }=Map, Valid) ->
    Mixins = lists:foldr(fun (MixinId, Acc) ->
				 [ occi_category:parse_id(MixinId) | Acc ]
			 end, [], maps:get(<<"mixins">>, Map, [])),
    Kind = occi_models:category(KindId),
    case occi_kind:has_parent(resource, Kind) of
	true ->
	    p_resource(Id, Kind, Mixins, Map, Valid);
	false ->
	    p_link(Id, Kind, Mixins, Map, Valid)
    end;

p_entity2(_Id, _Map, _Valid) ->
    throw({parse_error, missing_kind}).


p_resource(Id, Kind, Mixins, Map, Valid) ->
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
    R2 = occi_entity:set(Attributes, Valid, R1),
    p_resource_links(maps:get(<<"links">>, Map, []), Valid, R2).


p_resource_links([], _Valid, R) ->
    R;

p_resource_links([ Link | Tail ], Valid, R) ->
    Source = #{ <<"location">> => occi_resource:id(R), 
		<<"kind">> => occi_resource:kind(R) },
    L = p_resource_link(Link#{ <<"source">> => Source }, Valid),
    p_resource_links(Tail, Valid, occi_resource:add_link(L, R)).


p_resource_link(#{ <<"id">> := Id }=Link, Valid) ->
    p_resource_link2(binary_to_list(Id), Link, Valid);

p_resource_link(_, _) ->
    throw({parse_error, missing_id}).


p_resource_link2(Id, #{ <<"kind">> := Kind }=Link, Valid) ->
    p_link(Id, Kind, maps:get(<<"mixins">>, Link, []), Link, Valid);

p_resource_link2(_, _, _) ->
    throw({parse_error, missing_kind}).


p_link(Id, Kind, Mixins,
       #{ <<"source">> := #{ <<"location">> := SourceLocation }=Source }=Map, Valid) ->
    p_link2(Id, Kind, Mixins, SourceLocation, Source, Map, Valid);

p_link(_Id, _Kind, _Mixins, _Map, _Valid) ->
    throw({parse_error, {link, missing_source}}).


p_link2(Id, Kind, Mixins, SourceLocation, Source, Map, Valid) when is_binary(SourceLocation) ->
    p_link2(Id, Kind, Mixins, binary_to_list(SourceLocation), Source, Map, Valid);

p_link2(Id, Kind, Mixins, SourceLocation, Source, 
	#{ <<"target">> := #{ <<"location">> := TargetLocation }=Target }=Map, Valid) ->
    SourceKind = maps:get(<<"kind">>, Source, undefined),
    TargetKind = maps:get(<<"kind">>, Target, undefined),
    Link = occi_link:new(Id, Kind,
			 SourceLocation, SourceKind, 
			 binary_to_list(TargetLocation), TargetKind),
    Link1 = lists:foldl(fun (MixinId, Acc) ->
				occi_resource:add_mixin(MixinId, Acc)
			end, Link, Mixins),
    Attributes = maps:merge(
		   maps:fold(fun (K, V, Acc) ->
				     Acc#{ binary_to_list(K) => V }
			     end, #{}, maps:get(<<"attributes">>, Map, #{})),
		   #{ "occi.core.title" => maps:get(<<"title">>, Map, <<>>) }),
    occi_link:set(Attributes, Valid, Link1);

p_link2(_, _, _, _, _, _, _) ->
    throw({parse_error, {link, missing_target}}).


%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
