%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_action).

-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2]}]).

-export([new/2]).

-type t() :: #{}.

-export_type([t/0]).

-spec new(Scheme :: string(), Term :: string()) -> t().
new(Scheme, Term) ->
    occi_category:new(Scheme, Term, action).
