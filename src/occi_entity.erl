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
	 attributes/1,
	 get/2,
	 set/3,
	 update/3,
	 is_subtype/2]).

-export([render/3]).

-include("occi_entity.hrl").
-include_lib("annotations/include/annotations.hrl").

-type entity() :: {
	      Class      :: occi_type:name(),
	      Id         :: string(),
	      Kind       :: occi_category:id(),
	      Mixins     :: [occi_category:id()],
	      Attributes :: maps:map(),
	      Values     :: maps:map()
	     }.


%% @doc Check type:
%% <ul>
%%   <li>server: immutable attributes can be changed, required ones must be set</li>
%%   <li>client: immutable attributes can not be set</li>
%%   <li>internal: for entity sub-types. Do not check if all required attributes are set (for instance in link constructor)</li>
%% </ul>
%% @end
-type validation() :: server | client | internal.

%% @doc opaque type representing an entity
%% @end
-type t() :: entity().
-export_type([t/0, validation/0]).

-define(entity_category_id, {"http://schemas.ogf.org/occi/core#", "entity"}).
-define(resource_category_id, {"http://schemas.ogf.org/occi/core#", "resource"}).
-define(link_category_id, {"http://schemas.ogf.org/occi/core#", "link"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


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


%% @doc Set the full list of attributes for this resource
%% 
%% All required attributes must be set.
%% @throws {invalid_keys, [occi_attribute:key()]} 
%%       | {invalid_value, [occi_attribute:key(), occi_base_type:t()]}
%%       | {immutable, [occi_attribute:key()]}
%%       | {required, [occi_attribute:key()]}
%% @end
%%-logging(debug).
-spec set(map(), validation(), t()) -> t().
set(Attrs, Validation, E) when is_map(Attrs) ->
    set_or_update(Attrs, Validation, E).


%% @doc Update attributes values.
%% 
%% @throws {invalid_keys, [occi_attribute:key()]} 
%%       | {invalid_value, [occi_attribute:key(), occi_base_type:t()]}
%%       | {required, [occi_attribute:key()]}
%% @end
%%-logging(debug).
-spec update(map(), validation(), t()) -> t().
update(Attrs, Validation, E) when is_map(Attrs) ->
    set_or_update(Attrs, Validation, E).


%% @doc Returns true if the entity is of type Type or subtype of
%% @end
-spec is_subtype(entity | resource | link, t()) -> boolean().
is_subtype(resource, E) when element(?class, E) =:= resource -> true;

is_subtype(link, E) when element(?class, E) =:= link -> true;

is_subtype(entity, _) -> true;

is_subtype(_, _) -> false.
    

%% @doc Render entity into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), uri:t()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).


%%%
%%% internal
%%%
spec(Key, E) ->
    Categories = maps:get(Key, element(?attributes, E)),
    occi_models:attribute(Key, Categories).


specs(E) ->
    maps:fold(fun (K, Categories, Acc) ->
		      Acc#{ K => occi_models:attribute(K, Categories) }
	      end, #{}, element(?attributes, E)).


set_or_update(Attrs, Validation, E) ->
    Specs = specs(E),
    Errors = lists:foldl(fun (K, Acc) ->
				 case maps:get(K, Specs, undefined) of
				     undefined ->
					 %% Check attribute exists
					 InvalidKeys = maps:get(invalid_keys, Acc, []),
					 maps:put(invalid_keys, [ K | InvalidKeys ], Acc);
				     Spec ->
					 %% If client validation, check attribute is mutable
					 is_allowed(K, Spec, Validation, Acc)
				 end
			 end, #{}, maps:keys(Attrs)),
    {Values, Errors2} =
	maps:fold(fun (K, V, {ValuesAcc, ErrorsAcc}) ->
			  set_value(K, V, maps:get(K, Specs), ValuesAcc, ErrorsAcc)
		  end, {element(?values, E), Errors}, Attrs),
    case Validation of 
	internal ->
	    return_or_errors(Values, Errors2, E);
	_ ->
	    check_required(Values, Errors2, Specs, E)
    end.


check_required(Values, Errors, Specs, E) ->
    Errors2 = 
	maps:fold(fun (K, undefined, Acc) ->
			  case occi_attribute:required(maps:get(K, Specs)) of
			      true -> 
				  RequiredKeys = maps:get(required, Acc, []),
				  maps:put(required, [ K | RequiredKeys ], Acc);
			      false ->
				  Acc
			  end;
		      (_, _, Acc) ->
			  Acc
		  end, Errors, element(?attributes, E)),
    return_or_errors(Values, Errors2, E).


return_or_errors(Values, Errors, E) ->
    case maps:size(Errors) of
	0 ->
	    setelement(?values, E, Values);
	_ ->
	    throw(maps:to_list(Errors))
    end.


is_allowed(_K, _Spec, internal, Errors) ->
    Errors;

is_allowed(_K, _Spec, server, Errors) ->
    Errors;

is_allowed(K, Spec, client, Errors) ->
    case occi_attribute:mutable(Spec) of
	true ->
	    Errors;
	false ->
	    Immutables = maps:get(immutables, Errors, []),
	    maps:put(immutables, [ K | Immutables ], Errors)
    end.


%%-logging(debug).
get_default(Key, E) ->
    occi_attribute:default(spec(Key, E)).


set_value(Key, undefined, _Spec, Values, Errors) ->
    {maps:put(Key, undefined, Values), Errors};

set_value(Key, Value, Spec, Values, Errors) ->
    try occi_base_type:cast(Value, occi_attribute:type(Spec)) of
	Casted ->
	    {maps:put(Key, Casted, Values), Errors}
    catch throw:_Err ->
	    InvalidValues = maps:get(invalid_values, Errors, []),
	    {Values, maps:put(invalid_values, [ {Key, Value} | InvalidValues ], Errors)}
    end.


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
