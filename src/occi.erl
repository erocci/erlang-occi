%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi).

-behaviour(application).

%% Application behaviour
-export([start/2,
	 stop/1]).

-type t_name() :: occi_extension
		| occi_category
		| occi_kind
		| occi_mixin
		| occi_action
		| occi_attribute
		| occi_entity
		| occi_resource
		| occi_link.

-type t() :: occi_extension:t()
	   | occi_category:t()
	   | occi_kind:t()
	   | occi_mixin:t()
	   | occi_action:t()
	   | occi_attribute:t()
	   | occi_entity:t()
	   | occi_resource:t()
	   | occi_link:t().

-export_type([t_name/0,
	      t/0]).

start(_Type, _Args) ->
    occi_sup:start_link().

stop(_State) ->
	ok.
