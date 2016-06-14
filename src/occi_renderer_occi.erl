%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_renderer_occi).

-include("occi_log.hrl").

-export([render/2]).


-spec render(T :: occi:t(), Ctx :: occi_uri:t()) -> proplists:proplist().
render(T, Ctx) ->
    H = occi_renderer_text:to_headers(occi_type:type(T), T, orddict:new(), Ctx),
    H2 = lists:foldl(fun ({_K, []}, Acc) ->
			     Acc;
			 ({K, Bins}, Acc) ->
			     [ [<<", ">>, Head] | Tail ] = [ [ <<", ">>, V ] || V <- Bins ],
			     [ {K, [ Head, Tail ]} | Acc ]
		     end, [], H),
    lists:reverse(H2).
