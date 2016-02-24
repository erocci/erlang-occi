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
	 id/1,
	 kind/1,
	 mixins/1,
	 add_mixin/2,
	 title/1,
	 title/2,
	 attributes/1,
	 get/2,
	 set/3]).

-type t() :: #{}.
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "entity"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {unknown_category, term()}
-spec new(uri:t()) -> t().
new(Id) ->
    new(Id, ?category_id).


%% @throws {unknown_category, term()}
-spec new(string(), occi_category:id() | string() | binary()) -> t().
new(Id, KindId) when is_list(Id), is_list(KindId); is_list(Id), is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId));

new(Id, {_Scheme, _Term}=CatId) ->
    Attrs = lists:foldl(fun (Attr, Acc) ->
				Acc#{ occi_attribute:name(Attr) => undefined }
			end, #{}, occi_models:attributes(CatId)),
    #{id => Id,
      kind => CatId,
      mixins => [],
      title => "",
      attributes => Attrs};

new(_, KindId) ->
    throw({unknown_category, KindId}).


-spec id(t()) -> uri:t().
id(E) ->
    maps:get(id, E).


-spec kind(t()) -> occi_kind:id().
kind(E) ->
    maps:get(kind, E).


-spec mixins(t()) -> [occi_mixin:id()].
mixins(E) ->
    maps:get(mixins, E).


-spec add_mixin(occi_category:id() | string() | binary(), t()) -> t().
add_mixin(MixinId, E) when is_list(MixinId); is_binary(MixinId) ->
    add_mixin(occi_category:parse_id(MixinId), E);

add_mixin(MixinId, E) ->
    Mixins = maps:get(mixins, E),
    Attrs = maps:fold(fun (K, _V, Acc) ->
			      case maps:is_key(K, Acc) of
				  true -> Acc;
				  false -> Acc#{ K => undefined }
			      end
		      end, maps:get(attributes, E), occi_models:attributes(MixinId)),
    E#{ mixins := [ MixinId | Mixins ], attributes := Attrs }.


-spec title(t()) -> string().
title(E) ->
    maps:get(title, E).


-spec title(string() | binary(), t()) -> t().
title(Title, E) when is_binary(Title) ->
    title(binary_to_list(Title), E);

title(Title, E) when is_list(Title) ->
    E#{ title := Title }.


-spec attributes(t()) -> map().
attributes(E) ->
    maps:get(attributes, E).


-spec get(occi_attribute:key(), t()) -> occi_attribute:value().
get(Key, E) ->
    try maps:get(Key, maps:get(attributes, E)) of
	Value -> Value
    catch error:{badkey, _} ->
	    get_default(Key, E)
    end.


%% @throws {invalid_key, occi_attribute:key()}
-spec set(occi_attribute:key(), occi_attribute:value(), t()) -> occi_attribute:value().
set(Key, Value, E) ->
    Attrs = maps:get(attributes, E),
    try maps:update(Key, Value, Attrs) of
	Attrs2 -> E#{ attributes := Attrs2 }
    catch error:{badkey, Key} ->
	    throw({invalid_key, Key})
    end.

%%%
%%% internal
%%%
get_default(Key, E) ->
    Categories = maps:get(mixins, E) ++ [maps:get(kind, E)],
    occi_attribute:default(occi_models:attribute(Key, Categories)).


%%%
%%% eunit
%%%
-ifdef(TEST).
new_test_() ->
    [
     ?_assertThrow({invalid_cid, ""}, new("http://example.org:8081/myentity0", ""))
    ].

-endif.
