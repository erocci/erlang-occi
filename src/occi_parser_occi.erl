%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_occi).


-export([parse/1]).

-spec parse(proplists:proplist()) -> occi_rendering:ast().
parse(Headers) ->
    orddict:fold(fun occi_parser_text:validate/3, #{}, occi_parser_http:parse(Headers)).
