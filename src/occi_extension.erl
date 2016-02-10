%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_extension).

-export([new/1,
	 name/1,
	 name/2,
	 scheme/1,
	 add_category/2,
	 kinds/1,
	 mixins/1,
	 add_import/2,
	 imports/1]).


-type id() :: string().
-type t() :: #{}.

-export_type([id/0, t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @throws {invalid_uri, string() | binary()}
-spec new(Scheme :: string() | binary()) -> t().
new(Scheme) when is_list(Scheme); is_binary(Scheme) ->
    try uri:from_string(Scheme) of
	_ ->
	    #{ 
	  name => "",
	  scheme => Scheme,
	  kinds => [],
	  mixins => [],
	  imports => []
	 }
    catch 
	error:{badmatch, _} ->
	    throw({invalid_uri, Scheme})
    end.


-spec name(t()) -> string().
name(E) ->
    maps:get(name, E).


-spec name(string(), t()) -> t().
name(Name, E) ->
    E#{ name := Name }.


-spec scheme(t()) -> id().
scheme(E) ->
    maps:get(scheme, E).


-spec add_category(occi_category:t(), t()) -> t().
add_category(Category, E) ->
    case occi_category:class(Category) of
	kind ->
	    E#{ kinds := [ Category | maps:get(kinds, E) ] };
	mixin ->
	    E#{ mixins := [ Category | maps:get(mixins, E) ] }
    end.


-spec kinds(t()) -> [occi_category:t()].
kinds(E) ->
    maps:get(kinds, E).


-spec mixins(t()) -> [occi_category:t()].
mixins(E) ->
    maps:get(mixins, E).


-spec add_import(string(), t()) -> t().
add_import(Scheme, E) ->
    E#{ imports := [ Scheme | maps:get(imports, E) ] }.


-spec imports(t()) -> [occi_extension:id()].
imports(E) ->
    maps:get(imports, E).


%%%
%%% eunit
%%%
-ifdef(TEST).
new_test_() ->
    E = new("http://example.org"),
    [
     ?_assertMatch("", name(E)),
     ?_assertMatch("http://example.org", scheme(E))
    ].

-endif.
