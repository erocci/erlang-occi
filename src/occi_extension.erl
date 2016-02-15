%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc An extension is a set of categories in the OCCI Core Model.
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_extension).

-include_lib("mixer/include/mixer.hrl").
-include("occi_log.hrl").

-export([new/1,
	 name/1,
	 name/2,
	 scheme/1,
	 add_category/2,
	 kinds/1,
	 mixins/1,
	 add_import/2,
	 imports/1]).

-export([load_path/1,
	 load/2,
	 render/2]).

-type id() :: string().
-type t() :: #{}.

-export_type([id/0, t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Creates an extension with a given scheme.
%% The scheme is used as namespace for categories.
%%
%% Throws extension if the scheme is not a valid URI.
%% @end
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

%% @doc Get the name of the extension.
%% @end
-spec name(t()) -> string().
name(E) ->
    maps:get(name, E).


%% @doc Set (optional) name of the extension
%% @end
-spec name(string(), t()) -> t().
name(Name, E) ->
    E#{ name := Name }.


%% @doc Get scheme of the extension
%% @end
-spec scheme(t()) -> id().
scheme(E) ->
    maps:get(scheme, E).


%% @doc Add a category (kind or mixin) to the extension.
%% Actions are contained within a kind or mixin.
%% @end
-spec add_category(occi_category:t(), t()) -> t().
add_category(Category, E) ->
    case occi_category:class(Category) of
	kind ->
	    E#{ kinds := [ Category | maps:get(kinds, E) ] };
	mixin ->
	    E#{ mixins := [ Category | maps:get(mixins, E) ] }
    end.


%% @doc Get the list of kinds of this extension
%% @end
-spec kinds(t()) -> [occi_category:t()].
kinds(E) ->
    maps:get(kinds, E).


%% @doc Get the list of mixins of this extension
%% @end
-spec mixins(t()) -> [occi_category:t()].
mixins(E) ->
    maps:get(mixins, E).


%% @doc Declare an extension to import
%%
%% WARNING: cycles are forbidden
%% @end
-spec add_import(string(), t()) -> t().
add_import(Scheme, E) ->
    E#{ imports := [ Scheme | maps:get(imports, E) ] }.


%% @doc Get list of imports
%% @end
-spec imports(t()) -> [occi_extension:id()].
imports(E) ->
    maps:get(imports, E).


%%%
%%% Rendering stubs
%%%
%%% @todo use parse_transform
%%%
load_path(Path) -> occi_rendering:load_path(?MODULE, Path).
load(MimeType, Path) -> occi_rendering:load(?MODULE, MimeType, Path).
render(MimeType, T) -> occi_rendering:render(?MODULE, MimeType, T).
    

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
