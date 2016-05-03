%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_uri).

-include("occi_uri.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{uri, except, [from_string/1,
		       to_string/1,
		       append_path/2]}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([urn/1, path_strip/1,
	 from_string/1,
	 from_string/2,
	 to_string/1,
	 to_string/2,
	 append_path/2]).

-record(uri, {scheme, user_info, host, port, path, q, frag, raw}).

-type t() :: #uri{}.

-export_type([t/0]).

%% @doc Parse uri
%% @end
-spec from_string(binary()) -> t().
from_string(<<"urn:", Rest/binary >>) ->
    uri:new(<<"urn">>, <<>>, <<>>, undefined, Rest, [], <<>>);

from_string(S) ->
    uri:from_string(S).


%% @doc Parse uri, eventually completing with host/port and path's 
%% prefix from context
%% @end
-spec from_string(binary() | string(), occi_ctx:t()) -> t().
from_string(Url, Ctx) when is_list(Url) ->
    from_string(list_to_binary(Url) ,Ctx);

from_string(<< "http://", _/binary >> = Url, _Ctx) ->
    uri:from_string(Url);

from_string(<< "https://", _/binary >> = Url, _Ctx) ->
    uri:from_string(Url);

from_string(<< $/, _/binary >> = Url, #{ url := Ctx }) when ?is_uri(Ctx)  ->
    uri:path(Ctx, Url);

from_string(Url, #{ url := Ctx }) when ?is_uri(Ctx), is_binary(Url)  ->
    append_path(Ctx, Url);

from_string(Url, _) ->
    from_string(Url).


%% @doc Render uri as binary
%% @end
-spec to_string(t()) -> binary().
to_string(#uri{ scheme = <<"urn">>, path = Path }) ->
    << "urn:", Path/binary >>;

to_string(U) ->
    uri:to_string(U).


%% @doc Render uri as binary, with a different context
%% @end
-spec to_string(uri:t(), occi_ctx:t()) -> binary().
to_string(#uri{ scheme = <<"urn">> }=Urn, _Ctx) ->
    to_string(Urn);

to_string(Uri, #{ url := Ctx }) when ?is_uri(Uri), ?is_uri(Ctx) ->
    to_string(uri:path(Ctx, uri:path(Uri)));

to_string(<< $/, Path/binary >>, #{ url := Ctx }) when ?is_uri(Ctx) ->
    to_string(uri:path(Ctx, << $/, Path/binary >>));

to_string(Path, #{ url := Ctx }) when is_binary(Path), ?is_uri(Ctx) ->
    to_string(append_path(Ctx, Path));

to_string(Uri, _) ->
    to_string(Uri).


%% @doc Given a (binary) seed, return a urn
%% @end
-spec urn(binary()) -> t().
urn(Seed) ->
    U = <<"urn:uuid:", (uuid:uuid_to_string(uuid:get_v3(oid, Seed), binary_standard))/binary >>,
    from_string(U).


%% @doc Append a path to the existing path of the system
-spec append_path(t(), binary()) -> t().
append_path(Uri, <<$/, NewPath/binary>>) ->
    append_path(Uri, NewPath);

append_path(Uri=#uri{path=Path}, NewPath) when is_binary(NewPath) ->
    Stripped = path_strip(Path),
    uri:path(Uri, <<Stripped/binary, $/, NewPath/binary>>).

%%%
%%% Priv
%%%
path_strip(<<>>) ->
    <<>>;

path_strip(<< $/ >>) ->
    <<>>;

path_strip(Bin) ->
    path_strip2(lists:reverse(binary_to_list(Bin))).


path_strip2([]) ->
    <<>>;

path_strip2([ $/ | Tail ]) ->
    path_strip2(Tail);

path_strip2(L) ->
    list_to_binary(lists:reverse(L)).


%%%
%%% eunit
%%%
-ifdef(TEST).
from_string2_test_() ->
    Ctx = occi_ctx:new(<<"http://localhost:8080/coll">>),
    [
     ?_assertMatch(#uri{ raw = <<"http://localhost:8080/coll/myresource">> }, 
		   from_string(<<"myresource">>, Ctx)),
     ?_assertMatch(#uri{ raw = <<"http://example.org/myresource">> }, 
		   from_string(<<"http://example.org/myresource">>, Ctx))
    ].

to_string2_test_() ->
    Ctx = occi_ctx:new(<<"http://localhost:8080">>),
    [
     ?_assertMatch(<<"http://localhost:8080/myresource">>, 
		     to_string(from_string(<<"http://example.org/myresource">>), Ctx))
    ].

append_path_test_() ->
    
    [
     ?_assertMatch(#uri{path = <<"/path">>}, 
		   append_path(from_string(<<"http://localhost">>), <<"/path">>)),
     ?_assertMatch(#uri{path = <<"/path">>}, 
		   append_path(from_string(<<"http://localhost/">>), <<"/path">>)),
     ?_assertMatch(#uri{path = <<"/path">>}, 
		   append_path(from_string(<<"http://localhost/">>), <<"path">>)),
     ?_assertMatch(#uri{path = <<"/super/path">>}, 
		   append_path(from_string(<<"http://localhost/super">>), <<"path">>)),
     ?_assertMatch(#uri{path = <<"/super/path">>}, 
		   append_path(from_string(<<"http://localhost/super/">>), <<"path">>)),
     ?_assertMatch(#uri{path = <<"/super/path">>}, 
		   append_path(from_string(<<"http://localhost/super/">>), <<"/path">>))
    ].

path_strip_test_() ->
    [
     ?_assertMatch(<<"/my/path">>, path_strip(<<"/my/path/">>)),
     ?_assertMatch(<<"/my/path">>, path_strip(<<"/my/path//">>)),
     ?_assertMatch(<<"/my/path">>, path_strip(<<"/my/path">>))
    ].
-endif.
