%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DlMgr = #{ id => occi_dl, start => {occi_dl, start_link, []} },
    Models = #{ id => occi_models, start => {occi_models, start_link, []} },
    {ok, {{one_for_one, 1, 5}, [DlMgr, Models]}}.
