%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_type).

-type t() :: {enum, [atom()]}
	   | string
	   | integer
	   | float.

-export_type([t/0]).
