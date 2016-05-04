%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Download resources, handling multiple urls for a single resource.
%%% Queue requests for a same resource.
%%%
%%% @todo handle update only if modified
%%% @end
%%% Created : 11 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_dl).

-include("occi_log.hrl").
-include_lib("annotations/include/annotations.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
	 resource/2]).

%% gen_server callbacks
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3]).

-type dl_status_t() :: request | recv | imported.
-record(dl, {id                  :: term(),
	     urls    = []        :: [{http_uri:uri(), file:filename_all()}],
	     status              :: dl_status_t(),
	     retry   = 0         :: integer(),
	     dev     = undefined :: file:io_device(),
	     pending             :: [pid()],
	     ref                 :: reference(),
	     request             :: reference()}).
-type dl() :: #dl{}.

-type state() :: ets:tid().

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ok = occi_utils:mkdir(occi_utils:resources_dir()),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec resource(term(), [http_uri:uri()]) -> {ok, file:filename_all()} | {error, term()}.
resource(Id, Urls) ->
    case gen_server:call(?MODULE, {import, Id, Urls}) of
	{imported, Res} ->
	    ?debug("Resource already imported: ~s", [Id]),
	    {ok, Res};
	{pending, Ref} ->
	    ?debug("Waiting for resource: ~s", [Id]),
	    wait_import(Id, Ref);
	{error, _}=Err ->
	    Err
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(term()) -> {ok, state()}.
init([]) ->
    {ok, ets:new(?MODULE, [set, private, {keypos, 2}])}.


handle_info({http, {RequestID, stream_start, _Headers}}, S) ->
    case ets:match_object(S, #dl{request=RequestID, _='_'}) of
	[] ->
	    {stop, unknown_request, S};
	[Res] ->
	    try begin
		    Dev = open_file(dl_path(Res)),
		    true = ets:insert(S, dl_recv(Dev, Res)),
		    {noreply, S}
		end
	    catch throw:Err ->
		    ?error("Error downloading extension ~s: ~p", [dl_id(Res), Err]),
		    cancel_or_retry(Res, S)
	    end
    end;

handle_info({http, {RequestID, stream, Bin}}, S) ->
    case ets:match_object(S, #dl{request=RequestID, _='_'}) of
	[] ->
	    {stop, unknown_request, S};
	[Res] ->
	    case file:write(dl_dev(Res), Bin) of
		ok ->
		    {noreply, S};
		{error, _} = Err ->
		    ?error("Error downloading extension ~s: ~p", [dl_id(Res), Err]),
		    ok = file:delete(dl_path(Res)),
		    cancel_or_retry(Res, S)
	    end
    end;

handle_info({http, {RequestID, stream_end, _Headers}}, S) ->
    case ets:match_object(S, #dl{request=RequestID, _='_'}) of
	[] ->
	    {stop, unknown_request, S};
	[Dl] ->
	    ok = file:close(dl_dev(Dl)),
	    ok = send_pending({ok, dl_path(Dl)}, Dl),
	    true = ets:insert(S, dl_imported(Dl)),
	    {noreply, S}
    end;

handle_info({http, {RequestID, {error, Err}}}, S) ->
    case ets:match_object(S, #dl{request=RequestID, _='_'}) of
	[] ->
	    {stop, unknown_request, S};
	[Res] ->
	    ?debug("Error downloading extension ~s: ~p", [dl_id(Res), Err]),
	    cancel_or_retry(Res, S)
    end;

handle_info({http, {RequestID, {{_Version, 404, _Reason}, _Headers, _Body}}}, S) ->
    case ets:match_object(S, #dl{request=RequestID, _='_'}) of
	[] ->
	    {stop, unknown_request, S};
	[Dl] ->
	    case dl_next_url(Dl) of
		undefined ->
		    _ = send_pending({error, {not_found, dl_id(Dl)}}, Dl),
		    true = ets:delete(S, dl_id(Dl)),
		    {noreply, S};
		Dl2 ->
		    retry(Dl2, S)
	    end
    end;

handle_info({http, {RequestID, {{_Version, Status, _Reason}, _Headers, _Body}}}, S) 
  when Status >= 400, Status < 600 ->
    case ets:match_object(S, #dl{request=RequestID, _='_'}) of
	[] ->
	    {stop, unknown_request, S};
	[Dl] ->
	    _ = send_pending({error, {http_to_error(Status), dl_url(Dl)}}, Dl),
	    true = ets:delete(S, dl_id(Dl)),
	    {noreply, S}
    end;

handle_info(timeout, S) ->
    _ = send_all_pending({error, timeout}, S),
    {noreply, S}.


handle_call({import, Id, Urls}, {Pid, _Tag}=From, S) ->
    case ets:match_object(S, #dl{id=Id, _='_'}) of
	[] ->
	    maybe_request(Id, Urls, From, S);
	[Dl] ->
	    case dl_status(Dl) of
		imported ->
		    {reply, {imported, dl_path(Dl)}, S};
		request ->
		    true = ets:insert(S, dl_add_pending(Pid, Dl)),
		    {reply, {pending, dl_ref(Dl)}, S};
		recv ->
		    true = ets:insert(S, dl_add_pending(Pid, Dl)),
		    {reply, {pending, dl_ref(Dl)}, S}
	    end
    end.


handle_cast(_Evt, S) ->
    {noreply, S}.


terminate(Reason, S) ->
    send_all_pending({error, Reason}, S),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
maybe_request(Id, [ Url | _]=Urls, {Pid, Tag}, S) ->
    try begin
	    case dl_find(Urls) of
		undefined ->
		    RequestID = send_request(Url),
		    Dl = dl_new_request(Id, Urls, Pid, Tag, RequestID),
		    true = ets:insert(S, Dl),
		    {reply, {pending, Tag}, S, 5000};
		Url ->
		    Dl = dl_new_imported(Id, Url),
		    true = ets:insert(S, Dl),
		    {reply, {imported, dl_path(Dl)}, S}
	    end
	end
    catch throw:Err ->
	    {stop, Err, S}
    end.


send_request(Url) ->
    {ok, RequestID} = httpc:request(get, {Url, []}, [], [{sync, false}, {stream, self}]),
    RequestID.


open_file(Path) ->
    case file:open(Path, [write]) of
	{ok, Dev} -> Dev;
	{error, Err} -> throw({Err, Path})
    end.

cancel_or_retry(Dl, S) ->
    MaxRetry = application:get_env(occi, schemas_max_retry, 5),
    Retry = dl_retry(Dl),
    if 
	MaxRetry =< Retry ->
	    retry(Dl, S);
	true ->
	    ?error("Fetching extension ~s failed after ~b attempts. Try next url !", [dl_id(Dl), dl_retry(Dl)]),
	    case dl_next_url(Dl) of
		undefined ->
		    ok = send_pending({error, {not_found, dl_id(Dl)}}, Dl),
		    {noreply, ok, S};
		Dl2 ->
		    retry(Dl2, S)
	    end
    end.


retry(Dl, S) ->
    RequestID = send_request(dl_url(Dl)),
    true = ets:insert(S, dl_retry(RequestID, Dl)),
    {noreply, ok, S}.


wait_import(Id, Ref) ->
    receive
	{Ref, {ok, _}=Ret} -> 
	    ?debug("Resource received: ~s", [Id]),
	    Ret;
	{Ref, {error, Err}=Ret} ->
	    ?debug("Error receiving resource ~s: ~p", [Id, Err]),
	    Ret
    end.

send_pending(Res, Dl) ->
    Ref = dl_ref(Dl),
    lists:foreach(fun (Pid) ->
			  Pid ! {Ref, Res}
		  end, dl_pending(Dl)).


send_all_pending(Res, S) ->
    F = fun (Dl, Acc) ->
		_ = send_pending(Res, Dl),
		Acc
	end,
    _ = ets:foldl(F, [], S).


http_to_error(404) -> not_found;
http_to_error(Other) -> Other.

%%
%% dl structure functions
%% 
-spec dl_new_imported(term(), file:filename_all()) -> dl().
dl_new_imported(Id, Url) ->
    #dl{id=Id, status=imported, urls=[Url]}.


-spec dl_new_request(term(), [], pid(), reference(), reference()) -> dl().
dl_new_request(Id, Urls, Pid, Tag, RequestID) ->
    #dl{id=Id, urls=Urls, status=request, ref=Tag, pending=[Pid], request=RequestID}.


-spec dl_id(dl()) -> occi_extension:id().
dl_id(#dl{id=Scheme}) ->
    Scheme.


-spec dl_url(dl()) -> http_uri:uri().
dl_url(#dl{urls=[Url | _]}) ->
    Url.


-spec dl_path(dl()) -> file:filename_all().
dl_path(#dl{urls=[ Url | _ ]}) ->
    filename:join([occi_utils:resources_dir(), http_uri:decode(filename:basename(Url)) ]).


-spec dl_status(dl()) -> dl_status_t().
dl_status(#dl{status=Status}) ->
    Status.


-spec dl_add_pending(pid(), dl()) -> dl().
dl_add_pending(Pid, #dl{pending=Pids}=Dl) ->
    Dl#dl{pending=[Pid | Pids]}.


-spec dl_pending(dl()) -> [pid()].
dl_pending(#dl{pending=Pids}) when is_list(Pids) ->
    Pids;

dl_pending(_) ->
    [].


-spec dl_ref(#dl{}) -> reference().
dl_ref(#dl{ref=Tag}) ->
    Tag.


-spec dl_recv(file:io_device(), dl()) -> dl().
dl_recv(Dev, #dl{}=Dl) ->
    Dl#dl{status=recv, dev=Dev}.


-spec dl_imported(dl()) -> dl().
dl_imported(#dl{}=Dl) ->
    Dl#dl{status=imported, dev=undefined, pending=[], ref=undefined}.


-spec dl_retry(dl()) -> integer().
dl_retry(#dl{retry=Retry}) ->
    Retry.


dl_next_url(#dl{urls=[]}) ->
    undefined;

dl_next_url(#dl{urls=[ _ | Tail]}=Dl) ->
    Dl#dl{urls=Tail}.


-spec dl_retry(reference(), dl()) -> dl().
dl_retry(ReqID, #dl{retry=R}=Dl) ->
    Dl#dl{status=request, retry=R+1, request=ReqID}.


-spec dl_dev(dl()) -> file:io_device().
dl_dev(#dl{dev=Dev}) ->
    Dev.


dl_find([]) ->
    undefined;

dl_find([Url | Tail]) ->
    Path = filename:join([occi_utils:resources_dir(), http_uri:decode(filename:basename(Url))]),
    case filelib:wildcard(Path) of
	[] -> dl_find(Tail);
	[_] -> Url
    end.
