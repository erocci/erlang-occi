%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_xml).

-include("occi_log.hrl").
-include("occi_xml.hrl").
-include_lib("annotations/include/annotations.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([parse/2]).

-type state() :: #{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @throws {parse_error, term()}
-spec parse(occi:t_name(), iolist()) -> occi_extension:t().
parse(RootType, Xml) ->
    case xmerl_sax_parser:stream(Xml, options(fun handle_event/3)) of
	{ok, #{ stack := [ {RootType, Type} ] }, _Rest} -> 
	    Type;
	{ok, #{ stack := [ {Other, _} ] }, _Rest} ->
	    throw({parse_error, {invalid_type, Other}});
	{ok, Else, _Rest} ->
	    throw({parse_error, Else});
	{Tag, {_, _, LineNo}, Reason, _EndTags, _EventState}=Err -> 
	    ?error("[line ~b] parse error: {~p, ~p}", [LineNo, Tag, Reason]),
	    throw({parse_error, Err})
    end.

options(Fun) ->
    [{event_fun, Fun},
     {event_state, #{ stack => [], ns => #{} }}].


%% @throws {invalid_attribute_type, term()} | {unknown_attribute, string(), {string(), string(), integer()}}
-spec handle_event(xmerl_sax_parser:event(), term(), state()) -> state().
handle_event(startDocument, _, S) ->
    S;

handle_event(endDocument, _, S) ->
    S;

handle_event({startPrefixMapping, Prefix, URI}, _, #{ ns := NS }=S) ->
    S#{ ns := NS#{ URI => Prefix } };

handle_event({endPrefixMapping, _Prefix}, _, S) ->
    S;

handle_event({startElement, ?occi_uri, "extension", _QN, A}, _Pos, #{ stack := Stack }=S) ->
    Scheme = attr("scheme", A),
    Ext = occi_extension:new(Scheme),
    E2 = occi_extension:name(attr("name", A, ""), Ext),
    S#{ stack => [ {extension, E2} | Stack ] };

handle_event({startElement, ?occi_uri, "import", _QN, A}, _Pos, #{ stack := [ {extension, Ext} | Stack ] }=S) ->
    Scheme = attr("scheme", A),
    S#{ stack := [ {extension, occi_extension:add_import(Scheme, Ext)} | Stack ]};

handle_event({startElement, ?occi_uri, "kind", _QN, A}, _Pos, #{ stack := [ {extension, Ext} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A, occi_extension:scheme(Ext)),
    Title = attr("title", A, ""),
    Kind = occi_kind:title(Title, occi_kind:new(Scheme, Term)),
    S#{ stack := [ {kind, Kind}, {extension, Ext} | Stack ] };

handle_event({startElement, ?occi_uri, "mixin", _QN, A}, _Pos, #{ stack := [ {extension, Ext} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A, occi_extension:scheme(Ext)),
    Title = attr("title", A, ""),
    Mixin = occi_mixin:title(Title, occi_mixin:new(Scheme, Term)),
    S#{ stack := [ {mixin, Mixin}, {extension, Ext} | Stack ] };

handle_event({startElement, ?occi_uri, "depends", _QN, A}, _Pos, #{ stack := [ {mixin, Mixin} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    S#{ stack := [ {mixin, occi_mixin:add_depend({Scheme, Term}, Mixin)} | Stack ] };

handle_event({startElement, ?occi_uri, "applies", _QN, A}, _Pos, #{ stack := [ {mixin, Mixin} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    S#{ stack := [ {mixin, occi_mixin:add_apply({Scheme, Term}, Mixin)} | Stack ] };

handle_event({startElement, ?occi_uri, "action", _QN, A}, _Pos,
	     #{ stack := [ {Cls, Category}, {extension, Ext} | Stack] }=S) when Cls =:= kind; Cls =:= mixin ->
    Term = attr("term", A),
    Scheme = attr("scheme", A, occi_extension:scheme(Ext)),
    Title = attr("title", A, ""),
    Action = occi_action:title(Title, occi_action:new(Scheme, Term)),
    S#{ stack := [ {action, Action}, {Cls, Category}, {extension, Ext} | Stack ] };

handle_event({startElement, ?occi_uri, "parent", _QN, A}, _Pos, #{ stack := [ {kind, Kind} | Stack] }=S) ->
    Scheme = attr("scheme", A),
    Term = attr("term", A),
    Kind2 = occi_kind:parent({Scheme, Term}, Kind),
    S#{ stack := [ {kind, Kind2} | Stack ] };

handle_event({startElement, ?occi_uri, "attribute", _QN, A}, _Pos, 
	     #{ stack := [ {Cls, Category} | Stack] }=S) when Cls =:= kind; Cls =:= mixin; Cls =:= action ->
    Name = attr("name", A),
    Type = type_(attr("type", A, undefined), maps:get(ns, S)),
    Map = #{ title => attr("title", A, ""),
	     required => case attr("use", A, "optional") of
			     "optional" -> false;
			     "required" -> true
			 end,
	     mutable => case attr("immutable", A, "false") of
			    "true" -> false;
			     "false" -> true
			end,
	     default => attr("default", A, undefined) },
    S#{ stack := [ {attribute, Name, Type, Map}, {Cls, Category} | Stack ] };

handle_event({startElement, ?xsd_uri, "restriction", _QN, A}, _Pos,
	     #{ stack := [ {attribute, _, _, _}=Attribute | Stack] }=S) ->
    Type = case attr("base", A) of
	       "xs:string" -> {enum, []};
	       BaseType -> throw({invalid_attribute_type, BaseType})
	   end,
    S#{ stack := [ Type, Attribute | Stack ] };

handle_event({startElement, ?xsd_uri, "enumeration", _QN, A}, _Pos,
	     #{ stack := [ {enum, Enum} | Stack] }=S) ->
    Value = list_to_atom(attr("value", A)),
    S#{ stack := [ {enum, [ Value | Enum ]} | Stack ] };

handle_event({endElement, ?occi_uri, "kind", _QN}, _, #{ stack := [ {kind, Kind}, {extension, Ext} | Stack] }=S) ->
    Ext2 = occi_extension:add_category(Kind, Ext),
    S#{ stack := [ {extension, Ext2} | Stack ] };

handle_event({endElement, ?occi_uri, "mixin", _QN}, _, #{ stack := [ {mixin, Mixin}, {extension, Ext} | Stack] }=S) ->
    Ext2 = occi_extension:add_category(Mixin, Ext),
    S#{ stack := [ {extension, Ext2} | Stack ] };

handle_event({endElement, ?occi_uri, "depends", _QN}, _, S) ->
    S;

handle_event({endElement, ?occi_uri, "applies", _QN}, _, S) ->
    S;

handle_event({endElement, ?occi_uri, "attribute", _QN}, _, 
	     #{ stack := [ {attribute, _, undefined, _} | _] }) ->
    throw({invalid_attribute_type, "undefined"});

handle_event({endElement, ?occi_uri, "attribute", _QN}, _, 
	     #{ stack := [ {attribute, Name, Type, Map}, {Cls, Category} | Stack ] }=S) 
  when Cls =:= kind; Cls =:= mixin; Cls =:= action ->
    A = occi_attribute:new(Name, Type),
    A2 = occi_attribute:title(maps:get(title, Map), A),
    A3 = occi_attribute:required(maps:get(required, Map), A2),
    A4 = occi_attribute:mutable(maps:get(mutable, Map), A3),
    A5 = occi_attribute:default(maps:get(default, Map), A4),
    S#{ stack := [ {Cls, occi_category:add_attribute(A5, Category)} | Stack ] };

handle_event({endElement, ?xsd_uri, "restriction", _QN}, _, 
	     #{ stack := [ {enum, Values}, {attribute, Name, _, Map} | Stack ] }=S) ->
    S#{ stack := [ {attribute, Name, {enum, Values}, Map} | Stack ] };

handle_event({endElement, ?occi_uri, "action", _QN}, _, 
	     #{ stack := [ {action, Action}, {Cls, Category} | Stack ] }=S) ->
    S#{ stack := [ {Cls, occi_category:add_action(Action, Category)} | Stack ] };

handle_event({endElement, ?occi_uri, "import", _QN}, _, S) ->
    S;

handle_event({endElement, ?occi_uri, "parent", _QN}, _, S) ->
    S;

handle_event({endElement, ?xsd_uri, "enumeration", _QN}, _, S) ->
    S;

handle_event({endElement, ?occi_uri, "extension", _QN}, _, S) ->
    S;

handle_event({characters, _C}, _, S) ->
    S;

handle_event({ignorableWhitespace, _WS}, _, S) ->
    S;

handle_event({processingInstruction, _Target, _Data}, _, S) ->
    S;

handle_event({comment, _S}, _, S) ->
    S;

handle_event(startCDATA, _, S) ->
    S;

handle_event(endCDATA, _, S) ->
    S;

handle_event({startDTD, _Name, _PublicID, _SystemID}, _, S) ->
    S;

handle_event(endDTD, _, S) ->
    S;

handle_event({startEntity, _SysID}, _, S) ->
    S;

handle_event({endEntity, _SysID}, _, S) ->
    S;

handle_event({elementDecl, _Name, _Model}, _, S) ->
    S;

handle_event({attributeDecl, _Name, _AttrName, _Type, _Mode, _Value}, _, S) ->
    S;

handle_event({internalEntityDecl, _Name, _Value}, _, S) ->
    S;

handle_event({externalEntityDecl, _Name, _PublicID, _SystemID}, _, S) ->
    S;

handle_event({unparsedEntityDecl, _Name, _PublicId, _SystemId, _Ndata}, _, S) ->
    S;

handle_event({notationDecl, _Name, _PublicId, _SystemId}, _, S) ->
    S;

handle_event(Evt, _Pos, _S) ->
    throw({invalid_xml, io_lib:format("~p", [Evt])}).


attr(_Name, [], Default) ->
    Default;

attr(Name, [ {_URI, _Prefix, Name, Value} | _Attributes ], _Default) ->
    Value;

attr(Name, [ _Attr | Attributes ], Default) ->
    attr(Name, Attributes, Default).


attr(Name, Attributes) ->
    case attr(Name, Attributes, undefined) of
	undefined -> throw({unknown_attribute, Name});
	Value -> Value
    end.


type_(undefined, _NS) ->
    undefined;
type_(QN, NS) ->
    from_xsd_type(string:tokens(QN, ":"), NS).


from_xsd_type([Prefix, "string"], #{ ?xsd_uri := Prefix }) ->
    string;
from_xsd_type([Prefix, "integer"], #{ ?xsd_uri := Prefix }) ->
    integer;
from_xsd_type([Prefix, "float"], #{ ?xsd_uri := Prefix }) ->
    float;
from_xsd_type(Else, _) ->
    throw({invalid_type, Else}).


%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
