%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_category).

-export([new/1, 
	 new/2,
	 id/1,
	 title/1,
	 title/2,
	 attribute/2,
	 attributes/1]).

-export([parse_id/1]).

-type id() :: {Scheme :: string(), Term :: string()}.

-type t() :: #{}.

-export_type([id/0, t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {invalid_cid, term()}
-spec new(Id :: string() | id()) -> t().
new(CatId) when is_list(CatId); is_binary(CatId) ->
    new(parse_id(CatId));

new({Scheme, Term}) ->
    #{
     id => {Scheme, Term},
     title => "",
     attributes => #{}
    }.


%% throws {invalid_cid, {term(), term()}}
-spec new(Scheme :: string(), Term :: string()) -> t().
new(Scheme, Term) when is_list(Scheme), is_list(Term) ->
    new({Scheme, Term});

new(Scheme, Term) ->
    throw({invalid_cid, {Scheme ,Term}}).


-spec id(t()) -> occi_category:id().
id(C) ->
    maps:get(id, C).


-spec title(t()) -> string().
title(C) ->
    maps:get(title, C).


-spec title(string(), t()) -> t().
title(Title, C) ->
    C#{ title := Title }.


-spec attribute(occi_attribute:key(), t()) -> occi_attribute:t().
attribute(Key, T) ->
    Attrs = maps:get(attributes, T),
    map:get(Key, Attrs, undefined).


-spec attributes(occi_category:t()) -> map().
attributes(C) ->
    maps:get(attributes, C).


%% @throws {invalid_cid, term()}
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
