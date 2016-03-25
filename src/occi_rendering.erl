%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Provide common functions for rendering/parsing OCCI types.
%%%
%%% @end
%%% Created : 15 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_rendering).

-include("occi_rendering.hrl").

-export([load_model/4,
	 load_entity/4,
	 load_collection/3,
	 render/3]).

%% @doc Load the specified OCCI category or extension from an iolist()
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
%%
%% Supported mimetypes are:
%% <ul>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+json"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_model(occi_type:name(), occi_utils:mimetype(), iolist(), parse_ctx()) -> occi_type:t().
load_model(Type, MimeType, Bin, Ctx) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse_model(Type, Bin, Ctx)
    end.


%% @doc Load the specified OCCI entity (or sub-type thereof from an iolist()
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
%%
%% Supported mimetypes are:
%% <ul>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+json"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_entity(occi_type:name(), occi_utils:mimetype(), iolist(), parse_ctx()) -> occi_type:t().
load_entity(Type, MimeType, Bin, Ctx) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse_entity(Type, Bin, Ctx)
    end.


%% @doc Load the specified OCCI collection
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
%%
%% Supported mimetypes are:
%% <ul>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+json"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_collection(occi_utils:mimetype(), iolist(), parse_ctx()) -> occi_type:t().
load_collection(MimeType, Bin, Ctx) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse_collection(Bin, Ctx)
    end.


-spec render(occi_utils:mimetype(), occi_type:t(), render_ctx()) -> iolist().
render(MimeType, T, Ctx) ->
    case renderer(MimeType) of
	undefined ->
	    throw({unknown_mimetype, MimeType});
	Mod ->
	    Mod:render(T, Ctx)
    end.

%%%
%%% Priv
%%%
parser({<<"application">>, <<"xml">>, []})       -> occi_parser_xml;
parser({<<"application">>, <<"occi+xml">>, []})  -> occi_parser_xml;
parser(xml)                                      -> occi_parser_xml;
parser({<<"application">>, <<"json">>, []})      -> occi_parser_json;
parser({<<"application">>, <<"occi+json">>, []}) -> occi_parser_json;
parser(json)                                     -> occi_parser_json;
parser({<<"text">>, <<"plain">>, []})            -> occi_parser_text;
parser({<<"text">>, <<"occi+plain">>, []})       -> occi_parser_text;
parser({<<"text">>, <<"occi">>, []})             -> occi_parser_text;
parser(text)                                     -> occi_parser_text.


renderer({<<"application">>, <<"xml">>, []})       -> occi_renderer_xml;
renderer({<<"application">>, <<"occi+xml">>, []})  -> occi_renderer_xml;
renderer(xml)                                      -> occi_renderer_xml;
renderer({<<"application">>, <<"json">>, []})      -> occi_renderer_json;
renderer({<<"application">>, <<"occi+json">>, []}) -> occi_renderer_json;
renderer(json)                                     -> occi_renderer_json;
renderer({<<"text">>, <<"plain">>, []})            -> occi_renderer_text;
renderer({<<"text">>, <<"occi+plain">>, []})       -> occi_renderer_text;
renderer({<<"text">>, <<"occi">>, []})             -> occi_renderer_text;
renderer(text)                                     -> occi_renderer_text.
