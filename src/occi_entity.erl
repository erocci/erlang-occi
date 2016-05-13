%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Represents an OCCI entity.
%%%
%%% Uses maps for internal representation.
%%% Type checking is achieved with check/1. When setting an attribute,
%%% only attribute existence is checked.
%%%
%%% @todo Should type check when setting attribute ? (see renderings 
%%% where attributes can be set before categories declaration)
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_entity).

-include("occi_log.hrl").
-include("occi_entity.hrl").
-include("occi_type.hrl").

-export([id/1,
	 id/2,
	 location/1,
	 location/2,
	 kind/1,
	 mixins/1,
	 add_mixin/2,
	 rm_mixin/2,
	 attributes/1,
	 get/2,
	 set/3,
	 update/3,
	 actions/1,
	 do/3,
	 do/4,
	 is_subtype/2]).


-export([from_map/1,
	 from_map/2,
	 update_from_map/2,
	 render/3]).

-export([change_prefix/3]).

%% Internal (for subtypes)
-export([merge_parents/2, gen_location/2]).

-type entity() :: {
	      Class      :: occi_type:name(),
	      Id         :: binary(),
	      Location   :: occi_uri:url(),
	      Kind       :: occi_category:id(),
	      Mixins     :: [occi_category:id()],
	      Attributes :: maps:map(),
	      Values     :: maps:map(),
	      Actions    :: maps:map()
	     }.

-type validation() :: internal | client | server.

%% @doc opaque type representing an entity
%% @end
-opaque t() :: entity().
-type id() :: binary() | undefined.

-export_type([t/0, id/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec id(t()) -> id().
id(E) ->
    element(?id, E).


-spec id(id(), t()) -> t().
id(Id, E) ->
    setelement(?id, E, Id).


-spec location(t()) -> occi_uri:url().
location(E) ->
    element(?location, E).


-spec location(occi_uri:url(), t()) -> t().
location(Location, E) when is_binary(Location) ->
    setelement(?location, E, Location).


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
-spec add_mixin(occi_mixin:t() | occi_category:id() | string() | binary(), t()) -> t().
add_mixin(MixinId, E) when is_list(MixinId); is_binary(MixinId) ->
    add_mixin(occi_category:parse_id(MixinId), E);

add_mixin({_Scheme, _Term}=MixinId, E) ->
    case occi_models:category(MixinId) of
	undefined ->
	    throw({invalid_category, MixinId});
	Mixin ->
	    add_mixin(Mixin, E)
    end;

add_mixin(Mixin, E) when ?is_mixin(Mixin) ->
    merge_mixin(Mixin, setelement(?mixins, E, [ occi_mixin:id(Mixin) | element(?mixins, E) ])).


%% @doc Unassociate mixin from this entity
%% Attributes only defined by this mixin are removed
%%
%% @end
-spec rm_mixin(occi_mixin:t() | occi_category:id(), t()) -> t().
rm_mixin(Mixin, E) when ?is_mixin(Mixin) ->
    rm_mixin(occi_mixin:id(Mixin), E);

rm_mixin({_Scheme, _Term}=MixinId, E) ->
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
%% If attribute has default value, return the default value.
%% If attribute is not set and there is no default value, the attribute is not returned.
%% @end
-spec attributes(t()) -> map().
attributes(E) ->
    maps:fold(fun (Key, undefined, Acc) ->
		      Def = spec(Key, E),
		      case occi_attribute:default(Def) of
			  undefined -> Acc;
			  Default -> Acc#{ Key => Default }
		      end;
		  (Key, Value, Acc) ->
		      Acc#{ Key => Value }
	      end, #{}, element(?values, E)).


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
%% @throws {invalid_keys, [occi_attribute:key()]} | {invalid_value, [{occi_attribute:key(), occi_base_type:t()}]} | {immutable, [occi_attribute:key()]} | {required, [occi_attribute:key()]}
%% @end
-spec set(map(), validation(), t()) -> t().
set(Attrs, Validation, E) when is_map(Attrs) ->
    set_or_update(false, Attrs, Validation, E).


%% @doc Update attributes values.
%% 
%% @throws {invalid_keys, [occi_attribute:key()]} | {invalid_value, [{occi_attribute:key(), occi_base_type:t()}]} | {required, [occi_attribute:key()]}
%% @end
-spec update(map(), validation(), t()) -> t().
update(Attrs, Validation, E) when is_map(Attrs) ->
    set_or_update(true, Attrs, Validation, E).


%% @doc Return list of action ids
%% @end
-spec actions(t()) -> [occi_category:id()].
actions(E) ->
    Actions = element(?actions, E),
    maps:keys(Actions).


%% @doc Execute an action
%% `Fun = fun((ActionId :: occi_category:id(), Attributes :: maps:map(), Entity :: ()) -> {ok, t()} | {error, term()})'
%% @throws {invalid_action, occi_category:id()}
%% @end
-spec do(occi_categoy:id(), maps:map(), fun(), t()) -> t().
do(ActionId, Attributes, Fun, E) when ?is_category_id(ActionId) ->
    Invoke = occi_invoke:new(ActionId, Attributes),
    do(Invoke, Fun, E).


%% @equiv do(occi_invoke:id(Invoke), occi_invoke:attributes(Invoke), Fun, E)
%% @end
-spec do(occi_invoke:t(), fun(), t()) -> t().
do(Invoke, Fun, E) ->
    ActionId = occi_invoke:id(Invoke),
    case maps:is_key(ActionId, element(?actions, E)) of
	true ->
	    do_fun(ActionId, occi_invoke:attributes(Invoke), Fun, E);
	false ->
	    throw({invalid_action, ActionId})
    end.


%% @doc Returns true if the entity is of type Type or subtype of
%% @end
-spec is_subtype(entity | resource | link, t()) -> boolean().
is_subtype(resource, E) when element(?class, E) =:= resource -> true;

is_subtype(link, E) when element(?class, E) =:= link -> true;

is_subtype(entity, _) -> true;

is_subtype(_, _) -> false.


-spec from_map(occi_rendering:ast()) -> t().
from_map(Map) ->
    try begin
	    Kind = occi_models:kind(maps:get(kind, Map)),
	    from_map(Kind, Map)
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.


%% @doc New entity from AST
%% @end
-spec from_map(occi_category:t() | binary(), occi_rendering:ast()) -> t().
from_map(Kind, Map) when ?is_kind(Kind) ->
    Map1 = case maps:get(location, Map, undefined) of
	       undefined ->
		   Map#{ location => gen_location(maps:get(id, Map, undefined), Kind) };
	       _Location ->
		   Map
	   end,
    case occi_kind:known_parent(Kind) of
	resource -> occi_resource:from_map(Kind, Map1);
	link -> occi_link:from_map(Kind, Map1)
    end;

from_map(Path, Map) when is_binary(Path) ->
    try begin
	    Kind = occi_models:kind(maps:get(kind, Map)),
	    from_map(Kind, Map#{ location => Path })
	end
    catch throw:{badkey, _}=Err ->
	    throw(Err)
    end.


-spec update_from_map(occi_rendering:ast(), t()) -> t().
update_from_map(Map, Entity) ->
    try begin
	    Attrs0 = maps:get(attributes, Map, #{}),
	    Attrs1 = case maps:get(title, Map, undefined) of
			 undefined -> Attrs0;
			 Title -> Attrs0#{ <<"occi.core.title">> => Title }
		     end,
	    Attrs2 = case maps:get(summary, Map, undefined) of
			 undefined -> Attrs1;
			 Summary -> Attrs1#{ <<"occi.core.summary">> => Summary }
		     end,
	    try update(Attrs2, client, Entity) of
		_Entity2 -> maps:get(attributes, Map)
	    catch throw:Err2 -> throw(Err2)
	    end
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.


%% @doc Render entity into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), uri:t()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).


merge_parents(Kind, E) ->
    Attrs0 = occi_kind:attributes(Kind),
    Actions0 = occi_kind:actions(Kind),
    {Attrs1, Actions1} = lists:foldl(fun (ParentId, {AttrsAcc, ActionsAcc}) ->
					     case occi_models:category(ParentId) of
						 undefined ->
						     throw({invalid_category, ParentId});
						 Parent ->
						     { occi_kind:attributes(Parent) ++ AttrsAcc,
						       occi_kind:actions(Parent) ++ ActionsAcc }
					     end
				     end, {Attrs0, Actions0}, occi_kind:parents(Kind)),
    E1 = merge_attributes(lists:reverse(Attrs1), element(?attributes, E), element(?values, E), E),
    merge_actions(lists:reverse(Actions1), element(?actions, E1), E1).


%% @doc Change prefix urls in entity
%% @end
-spec change_prefix(occi_uri:prefix_op(), binary(), t()) -> t().
change_prefix(Op, Prefix, Entity) ->
    case element(?class, Entity) of
	resource ->
	    occi_resource:change_prefix(Op, Prefix, Entity);
	link ->
	    occi_link:change_prefix(Op, Prefix, Entity)
    end.


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


set_or_update(Update, Attrs, Validation, E) ->
    Specs = specs(E),
    Errors = lists:foldl(fun (K, Acc) ->
				 case maps:get(K, Specs, undefined) of
				     undefined ->
					 %% Check attribute exists
					 InvalidKeys = maps:get(invalid_keys, Acc, []),
					 maps:put(invalid_keys, [ K | InvalidKeys ], Acc);
				     Spec ->
					 if Update ->
						 %% If client validation, check attribute is mutable
						 is_allowed(K, Spec, Validation, Acc);
					    true ->
						 Acc
					 end
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


merge_mixin(Mixin, E) ->
    Depends = merge_depends(occi_mixin:depends(Mixin), orddict:from_list([{ 0, Mixin }])),
    {Attrs0, Actions0} = orddict:fold(fun (_Idx, Dep, {AttrsAcc, ActionsAcc}) ->
					      { occi_mixin:attributes(Dep) ++ AttrsAcc,
						occi_mixin:actions(Dep) ++ ActionsAcc }					   
				      end, {[], []}, Depends),
    E1 = merge_attributes(lists:reverse(Attrs0), element(?attributes, E), element(?values, E), E),
    merge_actions(lists:reverse(Actions0), element(?actions, E1), E1).


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


merge_actions([], Acc, E) ->
    setelement(?actions, E, Acc);

merge_actions([ Action | Tail ], Acc, E) ->
    Id = occi_action:id(Action),
    Category = occi_action:category(Action),
    Acc1 = maps:put(Id, [ Category | maps:get(Id, Acc, []) ], Acc),
    merge_actions(Tail, Acc1, E).


do_fun(ActionId, Attributes, Fun, E) ->
    try	Fun(ActionId, Attributes, E) of
	{ok, E2} ->
	    E2;
	{error, Err} ->
	    throw({do, Err})
    catch _:Err ->
	    throw({do, {internal, Err}})
    end.


gen_location(Id, Kind) when is_binary(Id) ->
    case valid_id(Id) of
	true -> 
	    << (occi_kind:location(Kind))/binary, $/, Id/binary >>;
	false ->
	    throw({invalid_id, Id})
    end;

gen_location(_, Kind) ->
    Uuid = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    << (occi_kind:location(Kind))/binary, $/, Uuid/binary >>.


-define(is_alpha(C), ((C >= 65 andalso C =< 90) orelse (C >= 97 andalso C =< 122))).
-define(is_digit(C), (C >= 48 andalso C =< 57)).

valid_id(<<>>) ->
    false;

valid_id(<< C, Rest/binary >>) when ?is_alpha(C) orelse ?is_digit(C) ->
    valid_id2(Rest);

valid_id(_) ->
    false.


valid_id2(<<>>) ->
    true;

valid_id2(<< C, Rest/binary >>) when ?is_alpha(C) 
				     orelse ?is_digit(C) 
				     orelse $- =:= C
				     orelse $_ =:= C
				     orelse $+ =:= C
				     orelse $: =:= C ->
    valid_id2(Rest);

valid_id2(_) ->
    false.



%% rel_id(#{ id := <<"/", _/binary >> =Path }=Map) ->
%%     << "/", Path1/binary >> = occi_utils:normalize(Path),
%%     Map#{ id  := Path1 };

%% rel_id(#{ id := << "http://", _/binary >> =Url }=Map) when is_binary(Url) ->
%%     url_id(Url, Map);

%% rel_id(#{ id := << "https://", _/binary >> =Url }=Map) when is_binary(Url) ->
%%     url_id(Url, Map);

%% rel_id(#{ id := << "urn:", _/binary >> =Url }=Map) when is_binary(Url) ->
%%     url_id(Url, Map);

%% rel_id(Map) ->
%%     Map.


%% url_id(Url, Map) ->
%%     << $/, Path/binary >> = occi_uri:path(occi_uri:from_string(Url)),
%%     Map#{ id := Path }.
