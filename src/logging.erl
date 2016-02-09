%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(logging).

-include("occi_log.hrl").
-include_lib("annotations/include/types.hrl").

-annotation('function').

-export([before_advice/4, 
	 after_advice/5]).

before_advice(#annotation{data=Lvl}, M, F, Inputs) ->
    log(Lvl, "call ~p:~p(~p)~n", [M, F, Inputs]),
    Inputs.

after_advice(#annotation{}, _M, _F, _Inputs, Result) ->
    Result.

log(debug, Message, Args) ->
    ?debug(Message, Args);
log(info, Message, Args) ->
    ?info(Message, Args).
