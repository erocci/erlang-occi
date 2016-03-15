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
parse_model(_Type, _Bin) when _Type =:= extension;
			      _Type =:= kind;
			      _Type =:= mixin;
			      _Type =:= actin ->
    ok.


-spec parse_entity(entity | resource | link, binary(), occi_entity:validation()) -> occi_type:t().
parse_entity(Type, Bin, Valid) when Type =:= resource;
				    Type =:= link ->
    Entity = p_entity(jsx:decode(Bin, [return_maps]), Valid),
    case occi_entity:is_subtype(Type, Entity) of
	true -> Entity;
	false -> throw({parse_error, {type, Entity}})
    end.


%%%
%%% Parsers
%%%
p_entity(#{ <<"id">> := Id }=Map, Valid) ->
    p_entity2(binary_to_list(Id), Map, Valid);
	
p_entity(_Map, _Valid) ->
    throw({parse_error, missing_id}).


p_entity2(Id, #{ <<"kind">> := KindId }=Map, Valid) ->
    Mixins = maps:get(<<"mixins">>, Map, []),
    E = occi_entity:new(Id, KindId),
    E1 = lists:foldl(fun (MixinId, Acc) ->
			     occi_entity:add_mixin(MixinId, Acc)
		     end, E, Mixins),
    p_entity3(E1, Map, Valid);

p_entity2(_Id, _Map, _Valid) ->
    throw({parse_error, missing_kind}).


p_entity3(E, Map, Valid) when element(?class, E) =:= resource ->
    p_resource(Map, Valid, E);

p_entity3(E, Map, Valid) when element(?class, E) =:= link ->
    p_link(Map, Valid, E).


p_resource(Map, Valid, E) ->
    Attributes = maps:merge(
		   maps:fold(fun (K, V, Acc) ->
				     Acc#{ binary_to_list(K) => V }
			     end, #{}, maps:get(<<"attributes">>, Map, #{})),
		   #{ "occi.core.summary" => maps:get(<<"summary">>, Map, <<>>),
		      "occi.core.title" => maps:get(<<"title">>, Map, <<>>) }),
    occi_entity:set(Attributes, Valid, E).


p_link(#{ <<"source">> := #{ <<"location">> := SourceLocation }=Source }=Map, Valid, E) ->
    p_link2(SourceLocation, Source, Map, Valid, E);

p_link(_Map, _Valid, _E) ->
    throw({parse_error, {link, missing_source}}).


p_link2(SourceLocation, Source, #{ <<"target">> := #{ <<"location">> := TargetLocation }=Target }=Map, Valid, E) ->
    A0 = maps:merge(
	   maps:fold(fun (K, V, Acc) ->
			     Acc#{ binary_to_list(K) => V }
		     end, #{}, maps:get(<<"attributes">>, Map, #{})),
	   #{ "occi.core.title" => maps:get(<<"title">>, Map, <<>>) }),
    A1 = case maps:get(<<"kind">>, Source, <<>>) of
	     <<>> ->
		 A0#{ "occi.core.source" => SourceLocation };
	     SourceKind ->
		 A0#{ "occi.core.source" => SourceLocation,
		      "occi.core.source.kind" => SourceKind }
	 end,
    A2 = case maps:get(<<"kind">>, Target, <<>>) of
	     <<>> ->
		 A1#{ "occi.core.target" => TargetLocation };
	     TargetKind ->
		 A1#{ "occi.core.target" => TargetLocation,
		      "occi.core.target.kind" => TargetKind }
	 end,
    occi_link:set(A2, Valid, E);

p_link2(_, _, _, _, _) ->
    throw({parse_error, {link, missing_target}}).


%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
