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
	 rm_mixin/2,
	 title/1,
	 title/2,
	 attributes/1,
	 get/2,
	 set/3]).

-include("occi_entity.hrl").
-include_lib("annotations/include/annotations.hrl").

-record entity, {
	  id               :: string(),
	  kind             :: occi_category:id(),
	  mixins     = []  :: [occi_category:id()],
	  attributes = #{} :: maps:map(),
	  values     = #{} :: maps:map()
	 }.

-type t() :: #entity{}.
-export_type([t/0]).

-define(entity_category_id, {"http://schemas.ogf.org/occi/core#", "entity"}).
-define(resource_category_id, {"http://schemas.ogf.org/occi/core#", "resource"}).
-define(link_category_id, {"http://schemas.ogf.org/occi/core#", "link"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {unknown_category, term()}
-spec new(string()) -> t().
new(Id) ->
    new(Id, ?entity_category_id).

%% @throws {unknown_category, term()}
-spec new(string(), occi_category:id() | string() | binary()) -> t().
new(Id, KindId) when is_list(Id), is_list(KindId); is_list(Id), is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId));

new(Id, {_Scheme, _Term}=CatId) ->
    case occi_models:category(CatId) of
	undefined ->
	    throw({unknown_category, CatId});
	C ->
	    Class = known_class([CatId | occi_kind:parents(C)]),
	    E = {Class, Id, CatId, [], #{}, #{}},
	    merge_parents(C, E)
    end.


-spec id(t()) -> string().
id(E) ->
    element(?id, E).


-spec kind(t()) -> occi_kind:id().
kind(E) ->
    element(?kind, E).


-spec mixins(t()) -> [occi_mixin:id()].
mixins(E) ->
    element(?mixins, E).


%% @doc Add a mixin to this entity
%% If an attribute is already defined, this mixin's definition take precedence over 
%% the previous one.
%%
%% @throws {invalid_category, occi_category:id()}
%% @end
-spec add_mixin(occi_category:id() | string() | binary(), t()) -> t().
add_mixin(MixinId, E) when is_list(MixinId); is_binary(MixinId) ->
    add_mixin(occi_category:parse_id(MixinId), E);

add_mixin(MixinId, E) ->
    case occi_models:category(MixinId) of
	undefined ->
	    throw({invalid_category, MixinId});
	Mixin ->
	    merge_mixin(Mixin, setelement(?mixins, E, [ MixinId | element(?mixins, E) ]))
    end.


%% @doc Unassociate mixin from this entity
%% Attributes only defined by this mixin are removed
%%
%% @end
-spec rm_mixin(occi_category:id(), t()) -> t().
rm_mixin(MixinId, E) ->
    Mixins = lists:filter(fun (Id) when Id =:= MixinId -> false;
			      (_) -> true
			  end, element(?mixins, E)),
    Attrs0 = element(?attributes, E),
    Values0 = element(?values, E),
    Keys = maps:keys(element(?attributes, E)),
    {Attrs, Values} = lists:foldl(fun (Name, {AccAttrs, AccValues}) ->
					  case lists:filter(fun (Id) when Id =:= MixinId -> false;
								(_) -> true
							    end, maps:get(Name, AccAttrs)) of
					      [] ->
						  { maps:remove(Name, AccAttrs), maps:remove(Name, AccValues) };
					      Categories ->
						  { maps:update(Name, Categories, AccAttrs), AccValues }
					  end
				  end, {Attrs0, Values0}, Keys),
    E0 = setelement(?attributes, E, Attrs),
    E1 = setelement(?values, E0, Values),
    setelement(?mixins, E1, Mixins).
		     
						      
-spec title(t()) -> string().
title(E) ->
    get("occi.core.title", E).


-spec title(string() | binary(), t()) -> t().
title(Title, E) ->
    set("occi.core.title", Title, E).


%% @doc Return key-value attributes map
-spec attributes(t()) -> map().
attributes(E) ->
    element(?values, E).


%% @throws {invalid_key, occi_attribute:key()}
-spec get(occi_attribute:key(), t()) -> occi_attribute:value() | undefined.
get(Key, E) ->
    try ?g(Key, E) of
	undefined -> get_default(Key, E);
	Value -> Value
    catch error:{badkey, _} ->
	    throw({invalid_key, Key})
    end.


%% @throws {invalid_key, occi_attribute:key()} | {invalid_value, occi_base_type:spec(), term()}
-spec set(occi_attribute:key(), occi_attribute:value(), t()) -> occi_attribute:value().
set(Key, Value, E) ->
    Attrs = element(?values, E),
    case maps:is_key(Key, Attrs) of
	true ->
	    Spec = spec(Key, E),
	    Casted = set_or_update(Value, maps:get(Key, Attrs), Spec),
	    setelement(?values, E, maps:update(Key, Casted, Attrs));
	false ->
	    throw({invalid_key, Key})
    end.

%%%
%%% internal
%%%
spec(Key, E) ->
    Categories = maps:get(Key, element(?attributes, E)),
    occi_models:attribute(Key, Categories).

-logging(debug).
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


merge_parents(Kind, E) ->
    Attrs0 = lists:foldl(fun (ParentId, Acc) ->
				 case occi_models:category(ParentId) of
				     undefined ->
					 throw({invalid_category, ParentId});
				     Parent ->
					 occi_kind:attributes(Parent) ++ Acc
				 end
			 end, occi_kind:attributes(Kind), occi_kind:parents(Kind)),
    merge_attributes(lists:reverse(Attrs0), element(?attributes, E), element(?values, E), E).


merge_mixin(Mixin, E) ->
    Depends = merge_depends(occi_mixin:depends(Mixin), orddict:from_list([{ 0, Mixin }])),
    Attrs0 = orddict:fold(fun (_Idx, Dep, Acc) ->
				  occi_mixin:attributes(Dep) ++ Acc
			  end, [], Depends),
    merge_attributes(lists:reverse(Attrs0), element(?attributes, E), element(?values, E), E).


merge_depends(Depends, Acc) ->
    {_Idx, Acc1} = merge_depends(Depends, 1, Acc),
    lists:reverse(Acc1).


%%-logging(debug).
merge_depends([], Idx, Acc) ->
    {Idx, Acc};

merge_depends([ DepId | Tail ], Idx, Acc) ->
    case occi_models:category(DepId) of
	undefined ->
	    throw({invalid_category, DepId});
	Dep ->
	    case occi_category:class(Dep) of
		mixin ->
		    Acc1 = orddict:store(Idx, Dep, Acc),
		    {Idx2, Acc2} = merge_depends(occi_mixin:depends(Dep), Idx+1, Acc1),
		    merge_depends(Tail, Idx2, Acc2);
		_ ->
		    throw({invalid_mixin, DepId})
	    end
    end.


%%-logging(debug).
merge_attributes([], AttrsAcc, ValuesAcc, E) ->
    E1 = setelement(?attributes, E, AttrsAcc),
    setelement(?values, E1, ValuesAcc);

merge_attributes([ Spec | Tail ], AttrsAcc, ValuesAcc, E) ->
    Name = occi_attribute:name(Spec),
    Category = occi_attribute:category(Spec),
    AttrsAcc1 = maps:put(Name, [ Category | maps:get(Name, AttrsAcc, []) ], AttrsAcc),
    ValuesAcc1 = maps:merge(#{ Name => undefined }, ValuesAcc),
    merge_attributes(Tail, AttrsAcc1, ValuesAcc1, E).


%% Use the module of the closest known ancestor
%% Clauses order is important !
known_class([]) ->                          entity;
known_class([?resource_category_id | _]) -> resource;
known_class([?link_category_id | _]) ->     link;
known_class([?entity_category_id | _]) ->   entity;
known_class([_ | Tail]) ->                  known_class(Tail).
