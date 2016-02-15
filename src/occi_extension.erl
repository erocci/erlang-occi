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

-export([load_path/1,
	 load/2]).


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


%%
%% Loading API
%%

%% @doc Load an extension from a file. 
%%
%% Mimetype is detected from file extension.
%%
%% Supported mimetypes are: xml
%% @throws enoent | eacces | eisdir | enotdir | enomem
-spec load_path(file:filename_all()) -> ok.
load_path(Filename) ->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    load(occi_utils:mimetype(Filename), Bin);
	{error, Reason} ->
	    throw(Reason)
    end.

%% @doc Load an extension from an iolist()
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
%%
%% Supported types are:
%% <ul>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+json"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()} | {unknown_extension, occi_extension:id()}
load(MimeType, Bin) when is_list(Bin); is_binary(Bin) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Ext = Mod:parse_extension(Bin),
	    load_extension(Ext)
    end.
	    

%%%
%%% Priv
%%%
load_extension(E) ->
    ok = load_imports(imports(E)),
    ok = load_categories(scheme(E), kinds(E)),
    ok = load_categories(scheme(E), mixins(E)).


%% Check extension exists in the database, throw error if not
load_imports([]) ->
    ok;

load_imports([ Scheme | Imports ]) ->
    ?debug("Import extension: ~s", [Scheme]),
    case import(Scheme) of
	{ok, Path} ->
	    ok = load_path(Path),
	    load_imports(Imports);
	{error, Err} ->
	    throw({import, Err})
    end.


load_categories(_Scheme, []) ->
    ok;

load_categories(Scheme, [ Cat | Categories ]) ->
    ?debug("Add category: ~p", [occi_category:id(Cat)]),
    ok = occi_models:add_category(Scheme, Cat),
    load_categories(Scheme, Categories).


import(Scheme) ->
    Base = baseurl(),
    Urls = [{Base ++ "/" ++ http_uri:encode(http_uri:encode(Scheme)) ++ ".xml", http_uri:encode(Scheme) ++ ".xml"}],
    occi_dl:resource(Scheme, Urls).


baseurl() ->
    case application:get_env(occi, schemas_baseurl, undefined) of
	undefined ->
	    throw({undefined_env, schemas_baseurl});
	{priv_dir, Dir} ->
	    filename:join([occi_utils:priv_dir(), Dir]);
	Dir when is_list(Dir) ->
	    Dir
    end.


parser({<<"application">>, <<"xml">>, []})       -> occi_parser_xml;
parser({<<"application">>, <<"occi+xml">>, []})  -> occi_parser_xml;
parser(xml)                                      -> occi_parser_xml;
parser({<<"application">>, <<"json">>, []})      -> occi_parser_json;
parser({<<"application">>, <<"occi+json">>, []}) -> occi_parser_json;
parser(json)                                     -> occi_parser_json.

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
