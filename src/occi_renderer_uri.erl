%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2013-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 11 April 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_uri).

-include("occi_uri.hrl").
-include("occi_log.hrl").
-include("occi_type.hrl").

%% API
-export([render/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec render(T :: occi:t(), Ctx :: occi_ctx:t()) -> iolist().
render(T, Ctx) when ?is_collection(T) ->
    lists:map(fun ({Id, _}) ->
		      [ occi_uri:to_string(Id, Ctx), $\n ]
	      end, occi_collection:elements(T));

render(T, _Ctx) -> 
    throw({bad_type, occi_type:type(T)}).
