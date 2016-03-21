%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_xml).

-include("occi_log.hrl").
-include("occi_xml.hrl").
-include("occi_rendering.hrl").
-include_lib("annotations/include/annotations.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([parse_model/3,
	 parse_entity/3]).

-type state() :: #{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec parse_model(extension | kind | mixin | action, iolist(), parse_ctx()) -> occi_type:t().
parse_model(RootType, Xml, Ctx) when RootType =:= extension;
				     RootType =:= kind;
				     RootType =:= mixin;
				     RootType =:= action ->
    case parse(Xml, Ctx) of
	{RootType, Type} ->
	    Type;
	{Other, _Type} ->
	    throw({parse_error, {invalid_type, Other}})
    end.


-spec parse_entity(entity | resource | link, iolist(), parse_ctx()) -> occi_type:t().
parse_entity(entity, Xml, Ctx) ->
    case parse(Xml, Ctx) of
	{entity, E} -> E;
	{resource, R} -> R;
	{link, L} -> L;
	{Other, _} -> throw({parse_error, {invalid_type, Other}})
    end;

parse_entity(resource, Xml, Valid) ->
    case parse(Xml, Valid) of
	{resource, R} -> R;
	{Other, _} -> throw({parse_error, {invalid_type, Other}})
    end;

parse_entity(link, Xml, Valid) ->
    case parse(Xml, Valid) of
	{link, L} -> L;
	{Other, _} -> throw({parse_error, {invalid_type, Other}})
    end.

%%%
%%% Internal
%%%

%% @throws {parse_error, integer(), term()}
-spec parse(iolist(), parse_ctx()) -> occi_type:t().
parse(Xml, Ctx) ->
    case xmerl_sax_parser:stream(Xml, options(fun handle_event/3, Ctx)) of
	{ok, {RootType, Type}, _Rest} -> 
	    {RootType, Type};
	{ok, Else, _Rest} ->
	    throw({parse_error, Else});
	{Tag, {_, _, LineNo}, Reason, _EndTags, _EventState} -> 
	    case Reason of
		R when is_list(R) ->
		    ?error("[line ~b] parse error: {~s, ~p}", [LineNo, Tag, Reason]);
		{Scheme, Term} ->
		    ?error("[line ~b] parse error: {~s, {~s, ~s}}", [LineNo, Tag, Scheme, Term]);
		_ ->
		    ?error("[line ~b] parse error: {~s, ~p}", [LineNo, Tag, Reason])
	    end,
	    throw({parse_error, LineNo, {Tag, Reason}})
    end.

options(Fun, #parse_ctx{ valid=Check, url=Url }) ->
    [{event_fun, Fun},
     {event_state, #{ stack => [], ns => #{}, check => Check, url => Url }}].


%% @throws {invalid_attribute_type, term()} | {unknown_attribute, string(), {string(), string(), integer()}}
%% @end
%%-logging(debug).
-spec handle_event(xmerl_sax_parser:event(), term(), state()) -> state().
handle_event(startDocument, _, S) ->
    S#{ stack := [ {document, undefined} ] };

handle_event(endDocument, _, #{ stack := [ {document, Doc} ]}) ->
    Doc;

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

%% extension can only be a root node
handle_event({endElement, ?occi_uri, "extension", _QN}, _, 
	     #{ stack := [ {extension, Ext},  {document, undefined} ] }=S) ->
    S#{ stack := [ {document, {extension, Ext} } ]};

handle_event({startElement, ?occi_uri, "import", _QN, A}, _Pos, #{ stack := [ {extension, Ext} | Stack ] }=S) ->
    Scheme = attr("scheme", A),
    S#{ stack := [ {extension, occi_extension:add_import(Scheme, Ext)} | Stack ]};

handle_event({endElement, ?occi_uri, "import", _QN}, _, S) ->
    S;

handle_event({startElement, ?occi_uri, "kind", _QN, A}, _Pos,
	     #{ stack := [ {extension, Ext} | Stack], url := Ctx }=S) ->
    Term = attr("term", A),
    K0 = occi_kind:new(attr("scheme", A, occi_extension:scheme(Ext)), Term),
    K1 = case attr("title", A, undefined) of
	     undefined ->
		 K0;
	     Title ->
		 occi_kind:title(Title, K0)
	 end,
    K2 = case Ctx of
	     undefined ->
		 K1;
	     _ ->
		 Location = occi_uri:to_abs(attr("location", A, Term), Ctx),
		 occi_kind:location(Location, K1)
	 end,
    S#{ stack := [ {kind, K2}, {extension, Ext} | Stack ] };

handle_event({startElement, ?occi_uri, "kind", _QN, A}, _Pos, 
	     #{ stack := [ {resource, Id, undefined, Map} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    S#{ stack := [ {resource, Id, {Scheme, Term}, Map} | Stack ] };

handle_event({startElement, ?occi_uri, "kind", _QN, A}, _Pos, 
	     #{ stack := [ {link, Id, undefined, Source, Target, Map} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    S#{ stack := [ {link, Id, {Scheme, Term}, Source, Target, Map} | Stack ] };

handle_event({endElement, ?occi_uri, "kind", _QN}, _, #{ stack := [ {kind, Kind}, {extension, Ext} | Stack] }=S) ->
    Ext2 = occi_extension:add_category(Kind, Ext),
    S#{ stack := [ {extension, Ext2} | Stack ] };

handle_event({endElement, ?occi_uri, "kind", _QN}, _, #{ stack := [ {resource, _, _, _} | _]=Stack }=S) ->
    S#{ stack := Stack };

handle_event({endElement, ?occi_uri, "kind", _QN}, _, #{ stack := [ {link, _, _, _, _, _} | _]=Stack }=S) ->
    S#{ stack := Stack };

handle_event({startElement, ?occi_uri, "mixin", _QN, A}, _Pos, 
	     #{ stack := [ {extension, Ext} | Stack], url := Ctx }=S) ->
    Term = attr("term", A),
    M0 = occi_mixin:new(attr("scheme", A, occi_extension:scheme(Ext)), Term),
    M1 = case attr("title", A, undefined) of
	     undefined ->
		 M0;
	     Title ->
		 occi_mixin:title(Title, M0)
	 end,
    M2 = case Ctx of 
	     undefined ->
		 M1;
	     _ ->
		 Location = occi_uri:to_abs(attr("location", A, Term), Ctx),
		 occi_mixin:location(Location, M1)
	 end,
    S#{ stack := [ {mixin, M2}, {extension, Ext} | Stack ] };

handle_event({startElement, ?occi_uri, "mixin", _QN, A}, _Pos, 
	     #{ stack := [ {resource, Id, Kind, Map} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    Map1 = Map#{ mixins := [ {Scheme, Term} | maps:get(mixins, Map) ]},
    S#{ stack := [ {resource, Id, Kind, Map1} | Stack ] };

handle_event({startElement, ?occi_uri, "mixin", _QN, A}, _Pos, 
	     #{ stack := [ {link, Id, Kind, Source, Target, Map} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    Map1 = Map#{ mixins := [ {Scheme, Term} | maps:get(mixins, Map) ]},
    S#{ stack := [ {link, Id, Kind, Source, Target, Map1} | Stack ] };

%% mixin is a root node
handle_event({startElement, ?occi_uri, "mixin", _QN, A}, _Pos, #{ stack := Stack, url := Ctx }=S) ->
    Term = attr("term", A),
    M0 = occi_mixin:new(attr("scheme", A), Term),
    M1 = occi_mixin:title(attr("title", A, ""), M0),
    Location = occi_uri:to_abs(attr("location", A, Term), Ctx),
    M2 = occi_mixin:location(Location, M1),
    S#{ stack := [ {mixin, M2} | Stack ] };

handle_event({endElement, ?occi_uri, "mixin", _QN}, _, #{ stack := [ {mixin, Mixin}, {extension, Ext} | Stack] }=S) ->
    Ext2 = occi_extension:add_category(Mixin, Ext),
    S#{ stack := [ {extension, Ext2} | Stack ] };

%% mixin is a root ndoe
handle_event({endElement, ?occi_uri, "mixin", _QN}, _, #{ stack := [ {mixin, Mixin}, {document, undefined} ] }=S) ->
    S#{ stack := [ {document, {mixin, Mixin} } ] };

handle_event({endElement, ?occi_uri, "mixin", _QN}, _, #{ stack := [ {resource, _, _, _} | _]=Stack }=S) ->
    S#{ stack := Stack };

handle_event({endElement, ?occi_uri, "mixin", _QN}, _, #{ stack := [ {link, _, _, _, _, _} | _]=Stack }=S) ->
    S#{ stack := Stack };

handle_event({startElement, ?occi_uri, "resource", _QN, A}, _Pos, #{ stack := Stack, url := Ctx }=S) ->
    Id = occi_uri:to_abs(attr("id", A), Ctx),
    Map = #{ links => [],
	     mixins => [],
	     attributes => #{ "occi.core.title" => attr("title", A, undefined) } },
    S#{ stack := [ {resource, Id, undefined, Map} | Stack ] };

%% resource can only be a root node
handle_event({endElement, ?occi_uri, "resource", _QN}, _,
	     #{ stack := [ {resource, Id, Kind, Map}, {document, undefined}], check := Check }=S) ->
    R = occi_resource:new(Id, Kind),
    R0 = lists:foldl(fun (MixinId, Acc) ->
			     occi_resource:add_mixin(MixinId, Acc)
		     end, R, maps:get(mixins, Map)),
    R1 = occi_resource:set(maps:get(attributes, Map), Check, R0),
    R2 = lists:foldl(fun (Link, Acc) ->
			     occi_resource:add_link(Link, Acc)
		     end, R1, maps:get(links, Map)),
    S#{ stack := [ {document, {resource, R2}} ] };

handle_event({startElement, ?occi_uri, "link", _QN, A}, _Pos, #{ stack := Stack, url := Ctx }=S) ->
    Id = occi_uri:to_abs(attr("id", A), Ctx),
    Target = occi_uri:to_abs(attr("target", A), Ctx),
    Source = case Stack of
		 [ {resource, ResId, _, _} | _ ] -> ResId;
		 _ -> occi_uri:to_abs(attr("source", A), Ctx)
	     end,
    Map = #{ attributes => #{ "occi.core.title" => attr("title", A, undefined) },
	     mixins => [] },
    S#{ stack := [ {link, Id, undefined, Source, Target, Map} | Stack ] };

%% link is child of resource
handle_event({endElement, ?occi_uri, "link", _QN}, _,
	     #{ stack := [ {link, Id, Kind, Source, Target, Map}, {resource, ResId, ResKind, ResMap} | Stack ], check := Check }=S) ->
    L = occi_link:new(Id, Kind, Source, ResKind, Target, undefined),
    L0 = lists:foldl(fun (MixinId, Acc) ->
			     occi_link:add_mixin(MixinId, Acc)
		     end, L, maps:get(mixins, Map)),
    L1 = occi_link:set(maps:get(attributes, Map), Check, L0),
    ResMap0 = ResMap#{ links := [ L1 | maps:get(links, ResMap) ]},
    S#{stack := [ {resource, ResId, ResKind, ResMap0} | Stack ]};

%% %% link is root node
handle_event({endElement, ?occi_uri, "link", _QN}, _,
 	     #{ stack := [ {link, Id, Kind, Source, Target, Map}, {document, undefined} ], check := Check }=S) ->
    L = occi_link:new(Id, Kind, Source, undefined, Target, undefined),
    L0 = lists:foldl(fun (MixinId, Acc) ->
			     occi_link:add_mixin(MixinId, Acc)
		     end, L, maps:get(mixins, Map)),
    L1 = occi_link:set(maps:get(attributes, Map), Check, L0),
    S#{stack := [ {document, {link, L1}} ]};

handle_event({startElement, ?occi_uri, "depends", _QN, A}, _Pos, #{ stack := [ {mixin, Mixin} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    S#{ stack := [ {mixin, occi_mixin:add_depend({Scheme, Term}, Mixin)} | Stack ] };

handle_event({endElement, ?occi_uri, "depends", _QN}, _, S) ->
    S;

handle_event({startElement, ?occi_uri, "applies", _QN, A}, _Pos, #{ stack := [ {mixin, Mixin} | Stack] }=S) ->
    Term = attr("term", A),
    Scheme = attr("scheme", A),
    S#{ stack := [ {mixin, occi_mixin:add_apply({Scheme, Term}, Mixin)} | Stack ] };

handle_event({endElement, ?occi_uri, "applies", _QN}, _, S) ->
    S;

handle_event({startElement, ?occi_uri, "action", _QN, A}, _Pos,
	     #{ stack := [ {Cls, Category}, {extension, Ext} | Stack] }=S) when Cls =:= kind; Cls =:= mixin ->
    Term = attr("term", A),
    Scheme = attr("scheme", A, occi_extension:scheme(Ext)),
    Title = attr("title", A, ""),
    Related = occi_category:id(Category),
    Action = occi_action:title(Title, occi_action:new(Scheme, Term, Related)),
    S#{ stack := [ {action, Action}, {Cls, Category}, {extension, Ext} | Stack ] };

handle_event({endElement, ?occi_uri, "action", _QN}, _, 
	     #{ stack := [ {action, Action}, {Cls, Category} | Stack ] }=S) ->
    S#{ stack := [ {Cls, occi_category:add_action(Action, Category)} | Stack ] };

handle_event({startElement, ?occi_uri, "parent", _QN, A}, _Pos, #{ stack := [ {kind, Kind} | Stack] }=S) ->
    Scheme = attr("scheme", A),
    Term = attr("term", A),
    Kind2 = occi_kind:parent({Scheme, Term}, Kind),
    S#{ stack := [ {kind, Kind2} | Stack ] };

handle_event({endElement, ?occi_uri, "parent", _QN}, _, S) ->
    S;

%% attribute spec
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
	     default => attr("default", A, undefined),
	     description => attr("default", A, "") },
    S#{ stack := [ {attribute, Name, Type, Map}, {Cls, Category} | Stack ] };

%% resource attribute instance
handle_event({startElement, ?occi_uri, "attribute", _QN, A}, _Pos, 
	     #{ stack := [ {resource, Id, Kind, Map} | Stack] }=S) ->
    Attrs0 = maps:put(attr("name", A), attr("value", A), maps:get(attributes, Map)),
    Map0 = Map#{ attributes := Attrs0 },
    S#{ stack := [ {resource, Id, Kind, Map0} | Stack ] };

%% link attribute instance
handle_event({startElement, ?occi_uri, "attribute", _QN, A}, _Pos, 
	     #{ stack := [ {link, Id, Kind, Source, Target, Map} | Stack] }=S) ->
    Attrs0 = maps:put(attr("name", A), attr("value", A), maps:get(attributes, Map)),
    Map0 = Map#{ attributes := Attrs0 },
    S#{ stack := [ {link, Id, Kind, Source, Target, Map0} | Stack ] };

handle_event({endElement, ?occi_uri, "attribute", _QN}, _, 
	     #{ stack := [ {attribute, _, undefined, _} | _] }) ->
    throw({invalid_attribute_type, "undefined"});

handle_event({endElement, ?occi_uri, "attribute", _QN}, _, 
	     #{ stack := [ {attribute, Name, Type, Map}, {Cls, Category} | Stack ] }=S) 
  when Cls =:= kind; Cls =:= mixin; Cls =:= action ->
    A = occi_attribute:new(occi_category:id(Category), Name, Type),
    A2 = occi_attribute:title(maps:get(title, Map), A),
    A3 = occi_attribute:required(maps:get(required, Map), A2),
    A4 = occi_attribute:mutable(maps:get(mutable, Map), A3),
    A5 = case maps:get(default, Map) of
	     undefined -> A4;
	     Default -> occi_attribute:default(Default, A4)
	 end,
    A6 = occi_attribute:description(maps:get(description, Map), A5),
    S#{ stack := [ {Cls, occi_category:add_attribute(A6, Category)} | Stack ] };

handle_event({endElement, ?occi_uri, "attribute", _QN}, _, 
	     #{ stack := [ Entity | Stack ] }=S) 
  when element(1, Entity) =:= resource; element(1, Entity) =:= link ->
    S#{ stack := [ Entity | Stack ] };

handle_event({startElement, ?xsd_uri, "restriction", _QN, A}, _Pos,
	     #{ stack := [ {attribute, _, _, _}=Attribute | Stack] }=S) ->
    Type = case attr("base", A) of
	       "xs:string" -> {enum, []};
	       BaseType -> throw({invalid_attribute_type, BaseType})
	   end,
    S#{ stack := [ Type, Attribute | Stack ] };

handle_event({endElement, ?xsd_uri, "restriction", _QN}, _, 
	     #{ stack := [ {enum, Values}, {attribute, Name, _, Map} | Stack ] }=S) ->
    S#{ stack := [ {attribute, Name, {enum, Values}, Map} | Stack ] };

handle_event({startElement, ?xsd_uri, "enumeration", _QN, A}, _Pos,
	     #{ stack := [ {enum, Enum} | Stack] }=S) ->
    Value = list_to_atom(attr("value", A)),
    S#{ stack := [ {enum, [ Value | Enum ]} | Stack ] };

handle_event({endElement, ?xsd_uri, "enumeration", _QN}, _, S) ->
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
    from_xml_type(string:tokens(QN, ":"), NS).


from_xml_type([Prefix, "string"], #{ ?xsd_uri := Prefix }) ->
    string;
from_xml_type([Prefix, "integer"], #{ ?xsd_uri := Prefix }) ->
    integer;
from_xml_type([Prefix, "float"], #{ ?xsd_uri := Prefix }) ->
    float;
from_xml_type([Prefix, "anyURI"], #{ ?xsd_uri := Prefix }) ->
    uri;
from_xml_type([Prefix, "kind"], #{ ?occi_uri := Prefix }) ->
    kind;
from_xml_type([Prefix, "resource"], #{ ?occi_uri := Prefix }) ->
    resource;
from_xml_type(Else, _) ->
    throw({invalid_type, Else}).


%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
