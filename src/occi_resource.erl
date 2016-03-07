%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_resource).

-include("occi_entity.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [new/1]},
	occi_type]).

-export([new/1]).

-export([load/3]).

-type t() :: occi_entity:t().
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "resource"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec new(string()) -> t().
new(Id) ->
    occi_entity:new(Id, ?category_id).


%% @doc Load resource from iolist 
%% @end
-spec load(occi_utils:mimetype(), iolist(), occi_entity:validation()) -> t().
load(Mimetype, Bin, V) -> 
    occi_rendering:load_entity(resource, Mimetype, Bin, V).

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
