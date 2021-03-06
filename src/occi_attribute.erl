%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_attribute).

-export([new/3,
	 name/1,
	 category/1,
	 type/1,
	 title/1,
	 title/2,
	 required/1,
	 required/2,
	 mutable/1,
	 mutable/2,
	 default/1,
	 default/2,
	 pattern/1,
	 pattern/2,
	 description/1,
	 description/2]).

-export([from_map/3]).

-type name_t() :: binary().
-type type_t() :: occi_base_type:t().
-type key() :: binary().
-opaque t() :: #{}.

-export_type([t/0, key/0]).

-spec new(Category :: occi_category:id(), Name :: key(), Type :: type_t()) -> t().
new(Category, Name, Type) when is_binary(Name) ->
    #{ 
     name => Name,
     category => Category,
     type => Type,
     title => <<>>,
     required => false,
     mutable => true,
     default => undefined,
     pattern => undefined,
     description => <<>>
    }.


-spec name(t()) -> name_t().
name(A) ->
    maps:get(name, A).


-spec category(t()) -> occi_category:id().
category(A) ->
    maps:get(category, A).


-spec type(t()) -> type_t().
type(A) ->
    maps:get(type, A).


-spec title(t()) -> binary().
title(A) ->
    maps:get(title, A).


-spec title(binary(), t()) -> t().
title(Title, A) when is_binary(Title) ->
    A#{ title := Title }.


-spec required(t()) -> boolean().
required(A) ->
    maps:get(required, A).


-spec required(boolean(), t()) -> t().
required(Required, A) when is_boolean(Required) ->
    A#{ required := Required }.


-spec mutable(t()) -> boolean().
mutable(A) ->
    maps:get(mutable, A).


-spec mutable(boolean(), t()) -> t().
mutable(Mutable, A) when is_boolean(Mutable) ->
    A#{ mutable := Mutable}.


-spec default(t()) -> occi_base_type:t().
default(A) ->
    maps:get(default, A).


-spec default(occi_base_type:t(), t()) -> t().
default(Value, #{ type := Type }=A) ->
    A#{ default := occi_base_type:cast(Value, Type) }.


-spec pattern(t()) -> binary().
pattern(A) ->
    maps:get(pattern, A).


-spec pattern(binary(), t()) -> t().
pattern(Pattern, A) when is_binary(Pattern) ->
    A#{ pattern := Pattern }.


-spec description(t()) -> binary().
description(A) ->
    maps:get(description, A).


-spec description(binary(), t()) -> t().
description(Desc, A) when is_binary(Desc) ->
    A#{ description := Desc }.

-spec from_map(Name :: binary(), CatId :: occi_category:id(), occi_rendering:ast()) -> t().
from_map(Name, CatId, Map) ->
    try begin
	    Type = maps:get(type, Map),
	    Spec = #{ name => Name,
		      category => CatId,
		      type => Type,
		      title => maps:get(title, Map, <<>>),
		      required => maps:get(required, Map, false),
		      mutable => maps:get(mutable, Map, true),
		      pattern => maps:get(pattern, Map, undefined),
		      description => maps:get(description, Map, <<>>)
		    },
	    case maps:get(default, Map, undefined) of
		undefined ->
		    Spec#{ default => undefined };
		Default ->
		    Spec#{ default => occi_base_type:cast(Default, Type) }
	    end
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.
