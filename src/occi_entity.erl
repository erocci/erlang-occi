%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Represents an OCCI entity.
%%%
%%% Uses maps for internal representation.
%%% Type checking is achieved with check/1. When setting an attribute,
%%% only attribute existence is checked.
%%%
%%% @end
%%%
%%% @todo Should type check when setting attribute ? (see renderings 
%%% where attributes can be set before categories declaration)
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_entity).

-export([new/1, 
	 new/2,
	 new/3,
	 id/1,
	 kind/1,
	 mixins/1,
	 add_mixin/2,
	 title/1,
	 title/2,
	 attributes/1,
	 get/2,
	 set/3]).

-include("occi_entity.hrl").

-record entity, {
	  id            :: string(),
	  kind          :: occi_category:id(),
	  mixins  = []  :: [occi_category:id()],
	  attributes = #{} :: maps:map()
	 }.

-type t() :: #entity{}.
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "entity"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {unknown_category, term()}
-spec new(uri:t()) -> t().
new(Id) ->
    new(Id, ?category_id, entity).

new(Id, Kind) ->
    new(Id, Kind, entity).

%% @throws {unknown_category, term()}
-spec new(string(), occi_category:id() | string() | binary(), entity | resource | link) -> t().
new(Id, KindId, Class) when is_list(Id), is_list(KindId); is_list(Id), is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId), Class);

new(Id, {_Scheme, _Term}=CatId, Class) when Class =:= entity; Class =:= resource; Class =:= link->
    Attrs = lists:foldl(fun (Attr, Acc) ->
				Acc#{ occi_attribute:name(Attr) => undefined }
			end, #{}, occi_models:attributes(CatId)),
    {Class, Id, CatId, [], Attrs#{ "title" => "" }};

new(_, _, Class) ->
    throw({invalid_entity, Class}).


-spec id(t()) -> uri:t().
id(E) ->
    element(?id, E).


-spec kind(t()) -> occi_kind:id().
kind(E) ->
    element(?kind, E).


-spec mixins(t()) -> [occi_mixin:id()].
mixins(E) ->
    element(?mixins, E).


-spec add_mixin(occi_category:id() | string() | binary(), t()) -> t().
add_mixin(MixinId, E) when is_list(MixinId); is_binary(MixinId) ->
    add_mixin(occi_category:parse_id(MixinId), E);

add_mixin(MixinId, E) ->
    Mixins = element(?mixins, E),
    Attrs = maps:fold(fun (K, _V, Acc) ->
			      case maps:is_key(K, Acc) of
				  true -> Acc;
				  false -> Acc#{ K => undefined }
			      end
		      end, element(?attributes, E), occi_models:attributes(MixinId)),
    setelement(?mixins, setelement(?attributes, E, Attrs), [MixinId | Mixins]).


-spec title(t()) -> string().
title(E) ->
    ?g("title", E).


-spec title(string() | binary(), t()) -> t().
title(Title, E) when is_binary(Title) ->
    title(binary_to_list(Title), E);

title(Title, E) when is_list(Title) ->
    ?s("title", Title, E).


-spec attributes(t()) -> map().
attributes(E) ->
    element(?attributes, E).


-spec get(occi_attribute:key(), t()) -> occi_attribute:value().
get(Key, E) ->
    try ?g(Key, E) of
	Value -> Value
    catch error:{badkey, _} ->
	    get_default(Key, E)
    end.


%% @throws {invalid_key, occi_attribute:key()} | {invalid_value, occi_base_type:spec(), term()}
-spec set(occi_attribute:key(), occi_attribute:value(), t()) -> occi_attribute:value().
set(Key, Value, E) ->
    Attrs = element(?attributes, E),
    case maps:is_key(Key, Attrs) of
	true ->
	    Spec = spec(Key, E),
	    Casted = set_or_update(Value, maps:get(Key, Attrs), Spec),
	    setelement(?attributes, E, maps:update(Key, Casted, Attrs));
	false ->
	    throw({invalid_key, Key})
    end.

%%%
%%% internal
%%%
spec(Key, E) ->
    Categories = element(?mixins, E) ++ [element(?kind, E)],
    occi_models:attribute(Key, Categories).

get_default(Key, E) ->
    occi_attribute:default(spec(Key, E)).


set_or_update(Value, undefined, Spec) ->
    occi_base_type:cast(Value, occi_attribute:type(Spec));

set_or_update(Value, _Else, Spec) ->
    update(Value, occi_attribute:mutable(Spec), Spec).


update(Value, true, Spec) ->
    occi_base_type:cast(Value, occi_attribute:type(Spec));

update(_Value, false, Spec) ->
    throw({immutable_attribute, occi_attribute:name(Spec)}).


%%%
%%% eunit
%%%
-ifdef(TEST).
new_test_() ->
    [
     ?_assertThrow({invalid_cid, ""}, new("http://example.org:8081/myentity0", ""))
    ].

-endif.
