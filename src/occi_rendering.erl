%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Provide common functions for rendering/parsing OCCI types.
%%%
%%% @end
%%% Created : 15 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_rendering).

-export([parse/2,
	 render/3]).

-type errors() :: {parse_error, term()}
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

-export_type([ast/0, errors/0]).

%% @doc Parse an OCCI object and returns it as a map.
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
-spec parse(occi_utils:mimetype(), iolist()) -> ast().
parse(Mimetype, Bin) ->
    (parser(Mimetype)):parse(Bin).


-spec render(occi_utils:mimetype(), occi_type:t(),occi_ctx:t()) -> iolist().
render(MimeType, T, Ctx) ->
    (renderer(MimeType)):render(T, Ctx).


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
