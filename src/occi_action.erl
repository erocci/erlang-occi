%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_action).

-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_category, except, [new/2]},
	occi_type]).

-export([new/2]).

-export([load/2]).

-record(action, { id :: occi_category:id(), m :: #{} }).
-type t() :: #action{}.

-export_type([t/0]).

-spec new(Scheme :: string(), Term :: string()) -> t().
new(Scheme, Term) ->
    occi_category:new(Scheme, Term, action).


%% @doc Load action from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist()) -> t().
load(Mimetype, Bin) -> 
    occi_rendering:load_model(action, Mimetype, Bin).
