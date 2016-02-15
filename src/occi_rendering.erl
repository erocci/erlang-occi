%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Provide common functions for rendering/parsing OCCI types.
%%%
%%% @end
%%% Created : 15 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_rendering).

-export([load_path/2,
	 load/3,
	 render/3]).


%% @doc Load the specified type from a file. 
%%
%% Mimetype is detected from file extension.
%%
%% Supported mimetypes are: xml
%% @throws enoent | eacces | eisdir | enotdir | enomem
-spec load_path(occi:t_name(), file:filename_all()) -> ok.
load_path(Type, Filename) ->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    load(Type, occi_utils:mimetype(Filename), Bin);
	{error, Reason} ->
	    throw(Reason)
    end.

%% @doc Load the specified OCCI type from an iolist()
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
-spec load(occi:t_name(), occi_utils:mimetype(), iolist()) -> occi:t().
load(Type, MimeType, Bin) when is_list(Bin); is_binary(Bin) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Mod:parse(Type, Bin)
    end.


-spec render(occi_t:name(), occi_utils:mimetype(), occi:t()) -> iolist().
render(Type, MimeType, T) ->
    case renderer(MimeType) of
	undefined ->
	    throw({unknown_mimetype, MimeType});
	Mod ->
	    Mod:render(Type, T)
    end.
	    

%%%
%%% Priv
%%%
parser({<<"application">>, <<"xml">>, []})       -> occi_parser_xml;
parser({<<"application">>, <<"occi+xml">>, []})  -> occi_parser_xml;
parser(xml)                                      -> occi_parser_xml;
parser({<<"application">>, <<"json">>, []})      -> occi_parser_json;
parser({<<"application">>, <<"occi+json">>, []}) -> occi_parser_json;
parser(json)                                     -> occi_parser_json.

renderer({<<"application">>, <<"xml">>, []})       -> occi_renderer_xml;
renderer({<<"application">>, <<"occi+xml">>, []})  -> occi_renderer_xml;
renderer(xml)                                      -> occi_renderer_xml;
renderer({<<"application">>, <<"json">>, []})      -> occi_renderer_json;
renderer({<<"application">>, <<"occi+json">>, []}) -> occi_renderer_json;
renderer(json)                                     -> occi_renderer_json.
