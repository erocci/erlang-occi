%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc 
%%%
%%% @end
%%% Created : 18 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_xml).

-include("occi_log.hrl").

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, empty_tag/2, export_text/1]).

-include_lib("xmerl/include/xmerl.hrl").

-define(indent, "  ").

'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V,Data];
'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>\n", Data].

%% The '#element#' function is the default handler for XML elements.
'#element#'(Tag, [], Attrs, _, _E) ->
    empty_tag(Tag, Attrs);

'#element#'(Tag, Data, Attrs, Parents, _E) ->
    Data1 = [ "\n", lists:duplicate(length(Parents)+1, ?indent),
	      Data, "\n", lists:duplicate(length(Parents), ?indent) ],
    markup(Tag, Attrs, Data1).
