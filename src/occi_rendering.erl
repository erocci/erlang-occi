%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Provide common functions for rendering/parsing OCCI types.
%%%
%%% @end
%%% Created : 15 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_rendering).

-export([load_model/3,
	 load_entity/3,
	 load_entity/4,
	 load_collection/3,
	 load_invoke/2,
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
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"plain"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"occi"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_model(occi_type:name(), occi_utils:mimetype(), iolist()) -> occi_type:t().
load_model(Type, MimeType, Bin) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse_model(Type, Bin)
    end.


%% @equiv load_entity(entity, Mimetype, Bin, Ctx)
%% @end
-spec load_entity(occi_utils:mimetype(), iolist(), occi_ctx:t()) -> occi_type:t().
load_entity(Mimetype, Bin, Ctx) ->
    load_entity(entity, Mimetype, Bin, Ctx).


%% @doc Load the specified OCCI entity (or sub-type thereof from an iolist()
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
%%
%% Supported mimetypes are:
%% <ul>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+xml"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"application"&gt;&gt;, &lt;&lt;"occi+json"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"plain"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"occi"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"uri-list"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_entity(occi_type:name(), occi_utils:mimetype(), iolist(), occi_ctx:t()) -> occi_type:t().
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
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"plain"&gt;&gt;, []}</li>
%%   <li>{&lt;&lt;"text"&gt;&gt;, &lt;&lt;"occi"&gt;&gt;, []}</li>
%% </ul>
%% @end
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_collection(occi_utils:mimetype(), iolist(), occi_ctx:t()) -> occi_collection:t().
load_collection(MimeType, Bin, Ctx) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse_collection(Bin, Ctx)
    end.



%% @doc Load the specified OCCI action invocation
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
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
%% @throws {parse_error, occi_parser:errors()} | {unknown_mimetype, term()}
-spec load_invoke(occi_utils:mimetype(), iolist()) -> occi_invoke:t().
load_invoke(MimeType, Bin) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse_invoke(Bin)
    end.


-spec render(occi_utils:mimetype(), occi_type:t(),occi_ctx:t()) -> iolist().
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
