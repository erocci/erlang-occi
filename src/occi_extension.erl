%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc An extension is a set of categories in the OCCI Core Model.
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_extension).

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

-export([render/3,
	 from_map/1]).

-mixin([occi_type]).

-type id() :: binary().
-record(extension, {id :: id(), m :: #{}}).

-opaque t() :: #extension{}.

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
-spec new(Scheme :: binary()) -> t().
new(Scheme) when is_binary(Scheme) ->
    try uri:from_string(Scheme) of
	_ ->
	    #extension{
	       id = Scheme,
	       m = #{ 
		 name => <<>>,
		 kinds => [],
		 mixins => [],
		 imports => []
		}
	      }
    catch
	error:{badmatch, _} ->
	    throw({invalid_uri, Scheme})
    end.

%% @doc Get the name of the extension.
%% @end
-spec name(t()) -> binary().
name(#extension{m=M}) ->
    maps:get(name, M).


%% @doc Set (optional) name of the extension
%% @end
-spec name(binary(), t()) -> t().
name(Name, #extension{m=M}=E) ->
    E#extension{ m=M#{ name := Name } }.


%% @doc Get scheme of the extension
%% @end
-spec scheme(t()) -> id().
scheme(#extension{id=Scheme}) ->
    Scheme.


%% @doc Add a category (kind or mixin) to the extension.
%% Actions are contained within a kind or mixin.
%% @end
-spec add_category(occi_category:t(), t()) -> t().
add_category(Category, #extension{m=M}=E) ->
    M2 = case occi_category:class(Category) of
	     kind ->
		 M#{ kinds := [ Category | maps:get(kinds, M) ] };
	     mixin ->
		 M#{ mixins := [ Category | maps:get(mixins, M) ] }
	 end,
    E#extension{m=M2}.


%% @doc Get the list of kinds of this extension
%% @end
-spec kinds(t()) -> [occi_category:t()].
kinds(#extension{m=M}) ->
    maps:get(kinds, M).


%% @doc Get the list of mixins of this extension
%% @end
-spec mixins(t()) -> [occi_category:t()].
mixins(#extension{m=M}) ->
    maps:get(mixins, M).


%% @doc Declare an extension to import
%%
%% WARNING: cycles are forbidden
%% @end
-spec add_import(binary() | string(), t()) -> t().
add_import(Scheme, E) when is_list(Scheme) ->
    add_import(list_to_binary(Scheme), E);

add_import(Scheme, #extension{m=M}=E) ->
    M2 = M#{ imports := [ Scheme | maps:get(imports, M) ] },
    E#extension{m=M2}.


%% @doc Get list of imports
%% @end
-spec imports(t()) -> [occi_extension:id()].
imports(#extension{m=M}) ->
    maps:get(imports, M).


%% @doc Load extension from ast
%% @end
-spec from_map(occi_rendering:ast()) -> t().
from_map(Map) -> 
    try begin
	    E = new(maps:get(scheme, Map)),
	    E1 = name(maps:get(name, Map, <<>>), E),
	    E2 = lists:foldl(fun (Scheme, Acc0) ->
				     add_import(Scheme, Acc0)
			     end, E1, lists:reverse(maps:get(imports, Map, []))),
	    E3 = lists:foldl(fun (Map1, Acc1) ->
				     add_category(occi_kind:from_map(Map1), Acc1)
			     end, E2, maps:get(kinds, Map, [])),
	    lists:foldl(fun (Map2, Acc2) ->
				add_category(occi_mixin:from_map(Map2), Acc2)
			end, E3, maps:get(mixins, Map, []))
	end
    catch error:{badkey, _}=Err ->
	    throw(Err)
    end.


%% @doc Render extension into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), occi_uri:t()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).

%%%
%%% eunit
%%%
-ifdef(TEST).
new_test_() ->
    E = new(<<"http://example.org">>),
    [
     ?_assertMatch(<<>>, name(E)),
     ?_assertMatch(<<"http://example.org">>, scheme(E))
    ].

-endif.
