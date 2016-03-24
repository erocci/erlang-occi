%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_uri).

-include("occi_rendering.hrl").
-include("occi_uri.hrl").

-mixin({uri, except, [from_string/1,
		      to_string/1,
		      append_path/2]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([urn/1,
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
-spec from_string(binary(), t()) -> t().
from_string(Url, undefined) ->
    from_string(Url);

from_string(<< $/, _/binary >> =Url, Ctx) when ?is_uri(Ctx)  ->
    uri:path(Ctx, Url);

from_string(<< "http://", _/binary >> = Url, _Ctx) ->
    uri:from_string(Url);

from_string(<< "https://", _/binary >> = Url, _Ctx) ->
    uri:from_string(Url);

from_string(Url, Ctx) when ?is_uri(Ctx), is_binary(Url)  ->
    uri:append_path(Ctx, Url).


%% @doc Render uri as binary
%% @end
-spec to_string(t()) -> binary().
to_string(#uri{ scheme = <<"urn">>, path = Path }) ->
    << "urn:", Path/binary >>;

to_string(U) ->
    uri:to_string(U).


%% @doc Render uri as binary, with a different context
%% @end
-spec to_string(uri:t(), uri:t()) -> binary().
to_string(Uri, undefined) ->
    to_string(Uri);

to_string(#uri{ scheme = <<"urn">> }=Urn, _Ctx) ->
    to_string(Urn);

to_string(Uri, Ctx) when ?is_uri(Ctx) ->
    to_string(uri:path(Ctx, uri:path(Uri))).


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

append_path(Uri=#uri{path = <<>>}, NewPath) ->
    uri:path(Uri, << $/, NewPath/binary >>);

append_path(Uri=#uri{path=Path}, NewPath) when is_binary(NewPath) ->
    case binary:last(Path) of
	$/ ->
	    uri:path(Uri, <<Path/binary, NewPath/binary>>);
	_ ->
	    uri:path(Uri, <<Path/binary, $/, NewPath/binary>>)
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).
from_string2_test_() ->
    Ctx = from_string(<<"http://localhost:8080/coll">>),
    [
     ?_assertMatch(#uri{ raw = <<"http://localhost:8080/coll/myresource">> }, 
		   from_string(<<"myresource">>, Ctx)),
     ?_assertMatch(#uri{ raw = <<"http://example.org/myresource">> }, 
		   from_string(<<"http://example.org/myresource">>, Ctx))
    ].

to_string2_test_() ->
    Ctx = from_string(<<"http://localhost:8080">>),
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
-endif.
