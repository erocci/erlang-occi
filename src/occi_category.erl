%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_category).

-export([parse_id/1]).

-type id() :: {Scheme :: string(), Term :: string()}.
-type t() :: #{}.

-export_type([id/0, t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec parse_id(string() | binary()) -> id().
parse_id(Id) when is_binary(Id) ->
    parse_id(binary_to_list(Id));

parse_id(Id) when is_list(Id) ->
    try uri:from_string(Id) of
	Uri ->
	    Scheme = binary_to_list(uri:to_string(uri:frag(Uri, <<>>))) ++ "#",
	    Term = case uri:frag(Uri) of
		       <<>> -> throw({invalid_cid, Id});
		       T -> binary_to_list(T)
		   end,
	    Term = binary_to_list(uri:frag(Uri)),
	    {Scheme, Term}
    catch
	error:{badmatch, _} ->
	    throw({invalid_cid, Id})
    end;

parse_id(Id) ->
    throw({invalid_cid, Id}).

%%%
%%% eunit
%%%
-ifdef(TEST).
parse_id_test_() ->
    [
     ?_assertThrow({invalid_cid, ""}, parse_id("")),
     ?_assertThrow({invalid_cid, "bad_scheme"}, parse_id("bad_scheme")),
     ?_assertThrow({invalid_cid, "http://example.org/occi#"}, parse_id("http://example.org/occi#")),
     ?_assertMatch({"http://example.org/occi#", "t"}, 
		   parse_id("http://example.org/occi#t")),
     ?_assertMatch({"http://example.org/occi#", "t"}, 
		   parse_id(<<"http://example.org/occi#t">>))
    ].

-endif.
