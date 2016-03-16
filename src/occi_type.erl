%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_type).

-export([type/1]).

-type name() :: extension
	      | categories
	      | category
	      | kind
	      | mixin
	      | action
	      | attribute
	      | entity
	      | resource
	      | link.

-type t() :: occi_extension:t()
	   | occi_category:t()
	   | occi_kind:t()
	   | occi_mixin:t()
	   | occi_action:t()
	   | occi_attribute:t()
	   | occi_entity:t()
	   | occi_resource:t()
	   | occi_link:t().

-export_type([name/0,
	      t/0]).


-spec type(t()) -> name().
type(T) when is_list(T) ->
    categories;

type(T) when is_tuple(T) ->
    element(1, T);

type(_) ->
    undefined.
