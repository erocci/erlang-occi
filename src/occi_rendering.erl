%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Provide common functions for rendering/parsing OCCI types.
%%%
%%% @end
%%% Created : 15 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_rendering).

-include("occi_types.hrl").

-export([parse/3,
	 parse_file/2,
	 parse_file/3,
	 render/3]).

-type error() :: {parse_error, term()}
	       | {unknown_mimetype, term()}
	       | {badkey, atom()}.

-type ast_key() :: categories
		 | attributes
		 | actions
		 | id
		 | links
		 | summary
		 | title
		 | action
		 | source
		 | target
		 | parent
		 | location
		 | depends
		 | applies.
-type ast_link_end() :: #{ location | kind => binary() }.
-type ast_value() :: binary() | list() | maps:map() | ast_link_end().

-type ast() :: #{ ast_key() => ast_value() }.

-export_type([ast/0, error/0]).

%% @doc Parse a binary and returns the object using `Mod:from_map/1'
%% or the provided function.
%%
%% Module must implement from_map/1
%%
%% Supported mimetypes are:
%% <ul>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"plain"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"occi"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws errors()
-spec parse(occi_utils:mimetype(), iolist(), occi_type:mod() | fun()) -> occi_type:t().
parse(Mimetype, Data, Fun) when is_function(Fun) ->
    Ast = (parser(Mimetype)):parse(Data),
    Fun(Ast);

parse(Mimetype, Data, Mod) when ?is_occi_mod(Mod) ->
    Mod:from_map((parser(Mimetype)):parse(Data)).


%% @doc Parse file and return an OCCI type
%% (Tries to) detects mimetype from filename
%% @end
-spec parse_file(file:filename_all(), occi_type:mod() | fun()) -> occi_type:t().
parse_file(Path, ModOrFun) ->
    parse_file(occi_utils:mimetype(Path), Path, ModOrFun).


%% @doc Parse file and return an OCCI type
%% (Tries to) detects mimetype from filename
%% @end
-spec parse_file(occi_utils:mimetype(), file:filename_all(), occi_type:mod() | fun()) -> occi_type:t().
parse_file(Mimetype, Path, ModOrFun) ->
    case file:read_file(Path) of
	{ok, Bin} ->
	    parse(Mimetype, Bin, ModOrFun);
	{error, Err} ->
	    throw(Err)
    end.


-spec render(occi_utils:mimetype(), occi_type:t(),occi_ctx:t()) -> iolist().
render(MimeType, T, Ctx) ->
    (renderer(MimeType)):render(T, Ctx).


%%%
%%% Priv
%%%
parser(<<"*/*">>)                                -> occi_parser_text;
parser({<<"application">>, <<"xml">>, _})        -> occi_parser_xml;
parser({<<"application">>, <<"occi+xml">>, _})   -> occi_parser_xml;
parser(xml)                                      -> occi_parser_xml;
parser({<<"application">>, <<"json">>, _})       -> occi_parser_json;
parser({<<"application">>, <<"occi+json">>, _})  -> occi_parser_json;
parser(json)                                     -> occi_parser_json;
parser({<<"text">>, <<"plain">>, _})             -> occi_parser_text;
parser({<<"text">>, <<"occi+plain">>, _})        -> occi_parser_text;
parser({<<"text">>, <<"occi">>, _})              -> occi_parser_text;
parser(text)                                     -> occi_parser_text.


renderer(<<"*/*">>)                              -> occi_renderer_text;
renderer({<<"application">>, <<"xml">>, _})      -> occi_renderer_xml;
renderer({<<"application">>, <<"occi+xml">>, _}) -> occi_renderer_xml;
renderer(xml)                                    -> occi_renderer_xml;
renderer({<<"application">>, <<"json">>, _})     -> occi_renderer_json;
renderer({<<"application">>, <<"occi+json">>, _})-> occi_renderer_json;
renderer(json)                                   -> occi_renderer_json;
renderer({<<"text">>, <<"plain">>, _})           -> occi_renderer_text;
renderer({<<"text">>, <<"occi+plain">>, _})      -> occi_renderer_text;
renderer({<<"text">>, <<"occi">>, _})            -> occi_renderer_text;
renderer(text)                                   -> occi_renderer_text;
renderer({<<"text">>, <<"uri-list">>, _})        -> occi_renderer_uri;
renderer('uri-list')                             -> occi_renderer_uri.
