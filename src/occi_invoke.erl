%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Implements action invocation
%%%
%%% @end
%%% Created : 25 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_invoke).

-include("occi_log.hrl").
-include("occi_rendering.hrl").
-include("occi_type.hrl").
-include_lib("annotations/include/annotations.hrl").

-export([new/2,
	 id/1,
	 title/1,
	 title/2,
	 attributes/1]).

-export([load/3,
	 render/3]).

-record(invoke, {id         :: occi_category:id(),
		 title      :: binary(),
		 apply      :: occi_category:id(),
		 attributes :: maps:map()}).

-type t() :: #invoke{}.

%% @doc Creates a new action invocation
%% @end
-spec new(occi_category:id(), maps:map()) -> t().
new(Id, Attributes) when ?is_category_id(Id), is_map(Attributes) ->
    Action = occi_models:action(Id),
    #invoke{id=Id, apply=occi_action:category(Action), attributes=set(Attributes, Action)}.


%% @doc Return action id
%% @end
-spec id(t()) -> occi_category:id().
id(#invoke{ id=Id }) ->
    Id.


%% @doc Return action title
%% @end
-spec title(t()) -> binary().
title(#invoke{ title=Title }) ->
    Title.


%% @doc Set action title
%% @end
-spec title(binary(), t()) -> t().
title(Title, #invoke{}=I) when is_binary(Title) ->
    I#invoke{ title=Title }.


%% @doc Get invocation attributes
%% @end
-spec attributes(t()) -> maps:map().
attributes(#invoke{attributes=Attributes}) ->
    Attributes.


%% @doc Load an action invocation from iolist
%% @end
load(Mimetype, Bin, Ctx) -> 
    occi_rendering:load_invoke(Mimetype, Bin, Ctx).


%% @doc Render action invocation into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), render_ctx()) -> iolist().
render(Mimetype, I, Ctx) ->
    occi_rendering:render(Mimetype, I, Ctx).


%%%
%%% Priv
%%%
set(Attributes, Def) ->
    Specs = lists:foldl(fun (Spec, Acc) ->
				[ { occi_attribute:name(Spec), Spec } | Acc ]
			end, [], occi_action:attributes(Def)),
    {V, E, S} = maps:fold(fun (K, V, {Acc, ErrAcc, SpecsAcc}) ->
				  case lists:keytake(K, 1, SpecsAcc) of
				      false ->
					  {Acc, add_error(invalid_keys, K, ErrAcc), SpecsAcc};
				      {value, {Name, Spec}, SpecsAcc2} ->
					  set_value(Name, V, Spec, Acc, ErrAcc, SpecsAcc2)
				  end
			  end, {#{}, #{}, Specs}, Attributes),
    defaults(V, E, S).


defaults(Values, Errors, Specs) ->
    {V1, E1} = lists:foldl(fun ({Name, Spec}, {VAcc, EAcc}) ->
				   default(Name, 
					   occi_attribute:default(Spec),
					   occi_attribute:required(Spec),
					   VAcc, EAcc)
			   end, {Values, Errors}, Specs),
    return_or_errors(V1, E1).


return_or_errors(Values, Errors) ->
    case maps:size(Errors) of
	0 ->
	    Values;
	_ ->
	    throw({maps:to_list(Errors)})
    end.
    

default(Name, undefined, true, Values, Errors) ->
    {Values, add_error(required, Name, Errors)};

default(_Name, undefined, false, Values, Errors) ->
    {Values, Errors};

default(Name, Default, _, Values, Errors) ->
    {maps:put(Name, Default, Values), Errors}.


add_error(Type, Err, Errors) ->
    Acc = maps:get(Type, Errors, []),
    maps:put(Type, [ Err | Acc ], Errors).


set_value(Name, Value, Spec, Values, Errors, Specs) ->
    try occi_base_type:cast(Value, occi_attribute:type(Spec)) of
	Casted ->
	    {maps:put(Name, Casted, Values), Errors, Specs}
    catch throw:_Err ->
	    {Values, add_error(invalid_values, {Name, Value}, Errors), Specs}
    end.
