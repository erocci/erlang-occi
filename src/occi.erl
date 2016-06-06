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
	 start_phase/3,
	 stop/1]).


start(_Type, _Args) ->
    occi_sup:start_link().

stop(_State) ->
	ok.


start_phase(mnesia, _Type, _Args) ->
    occi_models:init_mnesia().
