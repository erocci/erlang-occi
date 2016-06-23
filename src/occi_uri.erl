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

-export([from_string/1,
	 to_string/1,
	 to_string/2,
	 change_prefix/3,
	 canonical/1,
	 relative/2]).

-record(uri, {scheme, user_info, host, port, path, q, frag, raw}).

-type url() :: binary().
-type prefix_op() :: add | rm.

-opaque t() :: #uri{}.

-export_type([t/0, url/0, prefix_op/0]).

%% @doc Parse uri
%% @end
-spec from_string(binary()) -> t().
from_string(<<"urn:", Rest/binary >>) ->
    uri:new(<<"urn">>, <<>>, <<>>, undefined, Rest, [], <<>>);

from_string(S) ->
    uri:from_string(S).


%% @doc Make uri canonical: add default port if necessary
%% @end
-spec canonical(url()) -> url().
canonical(<< $/, _/binary >> =Bin) ->
    Bin;

canonical(Bin) ->
    Uri0 = from_string(Bin),
    Uri = case uri:port(Uri0) of
	      undefined ->
		  case uri:scheme(Uri0) of
		      <<"http">> -> uri:port(Uri0, 80);
		      <<"https">> -> uri:port(Uri0, 443);
		      _ -> Uri0
		  end;
	      _ -> Uri0
	  end,
    uri:to_string(Uri).


%% @doc Returns url without endpoint, if they match
%% @end
-spec relative(Endpoint :: url(), Url :: url()) -> url().
relative(Endpoint, Url) ->
    try canonical(Url) of
	Url0 ->
	    relative2(canonical(Endpoint), Url0)
    catch error:{badmatch, _} ->
	    Url
    end.
	

%% @doc Render uri as binary
%% @end
-spec to_string(t()) -> binary().
to_string(#uri{ scheme = <<"urn">>, path = Path }) ->
    << "urn:", Path/binary >>;

to_string(U) ->
    uri:to_string(U).


%% @doc Render uri as binary, with a different context
%% @end
-spec to_string(uri:t(), t()) -> binary().
to_string(#uri{ scheme = <<"urn">> }=Urn, _Ctx) ->
    to_string(Urn);

to_string(Uri, Ctx) when ?is_uri(Uri), ?is_uri(Ctx) ->
    to_string(uri:path(Ctx, uri:path(Uri)));

to_string(<< $/, Path/binary >>, Ctx) when ?is_uri(Ctx) ->
    to_string(uri:path(Ctx, << $/, Path/binary >>));

to_string(Path, Ctx) when is_binary(Path), ?is_uri(Ctx) ->
    to_string(append_path(Ctx, Path));

to_string(Uri, _) ->
    to_string(Uri).


%% @doc Append a path to the existing path of the system
-spec append_path(t(), binary()) -> t().
append_path(Uri, <<$/, NewPath/binary>>) ->
    append_path(Uri, NewPath);

append_path(Uri=#uri{path=Path}, NewPath) when is_binary(NewPath) ->
    Stripped = path_strip(Path),
    uri:path(Uri, <<Stripped/binary, $/, NewPath/binary>>).


%% @doc Change prefix of url
%% @end
-spec change_prefix(prefix_op(), binary(), binary()) -> binary().
change_prefix(_, _, undefined) ->
    undefined;

change_prefix(add, Prefix, Path) ->
    add_prefix(Prefix, Path);

change_prefix(rm, Prefix, Path) ->
    rm_prefix(Prefix, Path).


%% @doc Prepend path with prefix
%% @end
-spec add_prefix(binary(), binary()) -> binary().
add_prefix(Prefix, Path) when is_binary(Prefix), is_binary(Prefix) ->
    filename:join([Prefix, Path]).


%% @doc Delete prefix from path
%% @end
-spec rm_prefix(binary(), binary()) -> binary().
rm_prefix(<<>>, Path) ->
    rm_prefix2(Path);

rm_prefix(<< C, Rest/binary >>, << C, Path/binary >>) when is_binary(Path) ->
    rm_prefix(Rest, Path);

rm_prefix(Prefix, Path) when is_binary(Prefix), is_binary(Path) ->
    throw({mismatch, Prefix, Path}).


rm_prefix2(<< $/, Rest/binary >>) ->
    rm_prefix2(Rest);

rm_prefix2(Path) when is_binary(Path) ->
    Path.

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


relative2(_Endpoint, << $/, _/binary >> =Path) ->
    Path;

relative2(Endpoint, Path) ->
    relative3(Endpoint, Path).


relative3(<< $/ >>, << $/, _/binary >> =Path) ->
    Path;

relative3(<<>>, << $/, _/binary >> =Path) ->
    Path;

relative3(<< C, Rest/binary >>, << C, Path/binary >>) ->
    relative3(Rest, Path);

relative3(_, _) ->
    out_of_domain.


%%%
%%% eunit
%%%
-ifdef(TEST).
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

path_strip_test_() ->
    [
     ?_assertMatch(<<"/my/path">>, path_strip(<<"/my/path/">>)),
     ?_assertMatch(<<"/my/path">>, path_strip(<<"/my/path//">>)),
     ?_assertMatch(<<"/my/path">>, path_strip(<<"/my/path">>))
    ].

add_prefix_test_() ->
    [
     ?_assertMatch(<<"/my/prefixed/path">>, add_prefix(<<"/my/prefixed">>, <<"path">>))
    ].

rm_prefix_test_() ->
    [
     ?_assertMatch(<<"path">>, rm_prefix(<<"/my/prefix">>, <<"/my/prefix/path">>))
    ].

canonical_test_() ->
    [
     ?_assertMatch(<<"/path/to/somewhere">>, canonical(<<"/path/to/somewhere">>)),
     ?_assertMatch(<<"http://localhost:80/path/to/somewhere">>, 
		   canonical(<<"http://localhost/path/to/somewhere">>)),
     ?_assertMatch(<<"https://localhost:443/path/to/somewhere">>, 
		   canonical(<<"https://localhost/path/to/somewhere">>)),
     ?_assertMatch(<<"https://localhost:8080/path/to/somewhere">>, 
		   canonical(<<"https://localhost:8080/path/to/somewhere">>))     
    ].

relative_test_() ->
    [
     ?_assertMatch(<<"2e0063bc-fec4-4bf3-892d-8023908018f7">>,
		   relative(<<"http://localhost:8080">>, <<"2e0063bc-fec4-4bf3-892d-8023908018f7">>)),
     ?_assertMatch(<<"/path/to/a/resource">>,
		   relative(<<"http://localhost:8080">>, <<"http://localhost:8080/path/to/a/resource">>)),

     ?_assertMatch(<<"/path/to/a/resource">>,
		   relative(<<"http://localhost:80">>, <<"http://localhost/path/to/a/resource">>)),

     ?_assertMatch(<<"/path/to/a/resource">>,
		   relative(<<"https://localhost:443">>, <<"https://localhost/path/to/a/resource">>)),

     ?_assertMatch(<<"/path/to/a/resource">>,
		   relative(<<"http://localhost:8080">>, <<"/path/to/a/resource">>)),

     ?_assertMatch(out_of_domain,
		   relative(<<"http://localhost:80">>, <<"http://localhost:8080/path/to/a/resource">>)),

     ?_assertMatch(out_of_domain,
		   relative(<<"http://localhost:80">>, <<"http://example.org/path/to/a/resource">>))
    ].
-endif.
