%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_uri).

-include("occi_rendering.hrl").
-include("occi_uri.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([urn/1,
	 to_abs/2,
	 ctx/2]).

%% @doc Given a (binary) seed, return a urn
%% @end
-spec urn(binary()) -> binary().
urn(Seed) ->
    <<"urn:uuid:", (uuid:uuid_to_string(uuid:get_v3(oid, Seed), binary_standard))/binary >>.

%% @doc If url is an absolute path, prefix url with context's host
%% If url is a relative path, prefix url with context's path and host
%% If url is an url, returns url unchanged.
%% @end
-spec to_abs(binary() | string(), uri:t()) -> uri:t().
to_abs(Url, Ctx) when is_list(Url) ->
    to_abs(list_to_binary(Url), Ctx);

to_abs(<< $/, _/binary >> = Url, Ctx) when ?is_uri(Ctx)  ->
    uri:path(Ctx, Url);

to_abs(<< "http://", _/binary >> = Url, _Ctx) ->
    Url;

to_abs(<< "https://", _/binary >> = Url, _Ctx) ->
    Url;

to_abs(Url, Ctx) when ?is_uri(Ctx), is_binary(Url)  ->
    uri:append_path(Ctx, Url).


%% @doc Change url context (all but path)
%% @end
-spec ctx(uri:t(), render_ctx()) -> uri:t().
ctx(Uri, Ctx) ->
    uri:path(Ctx, uri:path(Uri)).

%%%
%%% eunit
%%%
-ifdef(TEST).
to_abs_test_() ->
    [
     ?_assertMatch(<<"http://localhost:8080/myresource">>, 
		   uri:to_string(to_abs("/myresource", uri:from_string(<<"http://localhost:8080/coll">>)))),
     ?_assertMatch(<<"http://localhost:8080/coll/myresource">>, 
		   uri:to_string(to_abs("myresource", uri:from_string(<<"http://localhost:8080/coll">>)))),
     ?_assertMatch(<<"http://example.org/myresource">>, 
		   uri:to_string(to_abs("http://example.org/myresource", uri:from_string(<<"http://localhost:8080/coll">>))))
    ].

ctx_test_() ->
    Ctx = uri:from_string(<<"http://localhost:8080">>),
    [
     ?_assertMatch(<<"http://localhost:8080/myresource", 
		   uri:to_string(ctx(uri_from_string(<<"http://example.org/myresource"), Ctx))
    ].
-endif.
