%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_xml).

-include("occi_log.hrl").
-include("occi_xml.hrl").
-include("occi_type.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([parse/1]).

-type state() :: #{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec parse(binary()) -> occi_rendering:ast().
parse(Bin) ->
    case xmerl_sax_parser:stream(Bin, options(fun handle_event/3)) of
	{ok, Ast, _Rest} -> 
	    Ast;
	{Tag, {_, _, LineNo}, Reason, _EndTags, _EventState} -> 
	    ?error("[line ~b] parse error: {~s, ~p}", [LineNo, Tag, Reason]),
	    throw({parse_error, LineNo, Reason})
    end.

%%%
%%% Internal
%%%
options(Fun) ->
    [{event_fun, Fun},
     {event_state, #{ parents => [], ns => #{} }}].


%% @throws {invalid_attribute_type, term()} | {unknown_attribute, string(), {string(), string(), integer()}}
%% @end
%%-logging(debug).
-spec handle_event(xmerl_sax_parser:event(), term(), state()) -> state().
handle_event(startDocument, _, S) ->
    S#{ parents := [ document ] };

handle_event(endDocument, _, #{ doc := Doc, parents := [ document ]}) ->
    Doc;

handle_event(endDocument, _, S) ->
    S;

handle_event({startPrefixMapping, Prefix, URI}, _, #{ ns := NS }=S) ->
    S#{ ns := NS#{ URI => Prefix } };

handle_event({endPrefixMapping, _Prefix}, _, S) ->
    S;

handle_event({startElement, RawNS, RawElement, _QN, Attributes}, Pos, #{ parents := Parents }=S) ->
    NS = ns_to_atom(RawNS),
    Element = el_to_atom(RawElement),
    ok = val_start_el(NS, Element, hd(Parents)),
    try handle_start_el(NS, Element, Attributes, Pos, S) of
	S1 ->
	    S1#{ parents := [ {NS, Element} | Parents ]}
    catch throw:Err ->
	    throw(Err);
	  _:Err ->
	    Error = iolist_to_binary(io_lib:format("Unexpected error: ~p", [Err])),
	    throw({xml, Error})
    end;

handle_event({endElement, RawNS, RawElement, _QN}, Pos, #{ parents := Parents }=S) ->
    NS = ns_to_atom(RawNS),
    Element = el_to_atom(RawElement),
    ok = val_end_el(NS, Element, hd(Parents)),
    try handle_end_el(NS, Element, Pos, S#{ parents := tl(Parents) } ) of
	S1 ->
	    S1
    catch throw:Err ->
	    throw(Err);
	  _:Err ->
	    Error = iolist_to_binary(io_lib:format("Unexpected error: ~p", [Err])),
	    throw({xml, Error})
    end;

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
    throw({xml, iolist_to_binary(io_lib:format("Unhandled event: ~p", [Evt]))}).


handle_start_el(occi, collection, _A, _Pos, S) ->
    S#{ collection => #{} };

handle_start_el(occi, extension, A, _Pos, S) ->
    Ext = #{ scheme => attr("scheme", A) },
    Ext1 = case attr("name", A, undefined) of
	       undefined -> Ext;
	       Name -> Ext#{ name => Name }
	   end,
    S#{ extension => Ext1 };

handle_start_el(occi, import,  A, _Pos, #{ extension := Ext }=S) ->
    S#{ extension => Ext#{ imports => [ attr("scheme", A) | maps:get(imports, Ext, []) ] } };

handle_start_el(occi, kind,  A, _Pos, #{ resource := Res, parents := [ {occi, resource} | _ ] }=S) ->
    S#{ resource => Res#{ kind => { attr("scheme", A), attr("term", A) } } };

handle_start_el(occi, kind,  A, _Pos, #{ link := Link, parents := [ {occi, link} | _ ] }=S) ->
    S#{ link => Link#{ kind => { attr("scheme", A), attr("term", A) } } };

handle_start_el(occi, kind, A, _Pos, #{ extension := Ext }=S) ->
    K0 = #{ term => attr("term", A), scheme => attr("scheme", A, maps:get(scheme, Ext)) },
    K1 = case attr("title", A, undefined) of
	     undefined -> K0;
	     Title -> K0#{ title => Title }
	 end,
    S#{ kind => K1 };

handle_start_el(occi, mixin, A, _Pos, #{ resource := Res, parents := [ {occi, resource} | _ ] }=S) ->
    Mixin = { attr("scheme", A), attr("term", A) },
    S#{ resource => Res#{ mixins => [ Mixin | maps:get(mixins, Res, []) ] } };

handle_start_el(occi, mixin, A, _Pos, #{ link := Link, parents := [ {occi, link} | _ ] }=S) ->
    Mixin = { attr("scheme", A), attr("term", A) },
    S#{ link => Link#{ mixins => [ Mixin | maps:get(mixins, Link, []) ] } };

handle_start_el(occi, mixin, A, _Pos, #{ extension := Ext, parents := [ {occi, extension} | _ ] }=S) ->
    M0 = #{ term => attr("term", A), scheme => attr("scheme", A, maps:get(scheme, Ext)) },
    M1 = case attr("title", A, undefined) of
	     undefined -> M0;
	     Title -> M0#{ title => Title }
	 end,
    S#{ mixin => M1 };

handle_start_el(occi, mixin, A, _Pos, S) ->
    M0 = #{ term => attr("term", A), scheme => attr("scheme", A) },
    M1 = case attr("title", A, undefined) of
	     undefined -> M0;
	     Title -> M0#{ title => Title }
	 end,
    S#{ mixin => M1 };

handle_start_el(occi, resource, A, _Pos, S) ->
    R0 = #{ id => attr("id", A),
	    attributes => #{ <<"occi.core.title">> => attr("title", A, undefined) }
	  },
    R1 = case attr("title", A, undefined) of
	     undefined -> R0;
	     Title -> R0#{ attributes => #{ <<"occi.core.title">> => Title } }
	 end,
    S#{ resource => R1 };

handle_start_el(occi, link, A, _Pos, S) ->
    L0 = #{ id => attr("id", A),
	    target => #{ location => attr("target", A) } },
    L1 = case attr("source", A, undefined) of
	     undefined -> L0;
	     Source -> L0#{ source => #{ location => Source } }
	 end,
    L2 = case attr("title", A, undefined) of
	     undefined -> L1;
	     Title -> L1#{ attributes => #{ <<"occi.core.title">> => Title } }
	 end,
    S#{ link => L2 };

handle_start_el(occi, depends, A, _Pos, #{ mixin := M }=S) ->
    Depend = { attr("scheme", A), attr("term", A) },
    S#{ mixin := M#{ depends => [ Depend | maps:get(depends, M, []) ] } };

handle_start_el(occi, applies, A, _Pos, #{ mixin := M }=S) ->
    Depend = { attr("scheme", A), attr("term", A) },
    S#{ mixin := M#{ applies => [ Depend | maps:get(applies, M, []) ] } };

handle_start_el(occi, action, A, _Pos, #{ parents := [ document ] }=S) ->
    Action = { attr("scheme", A), attr("term", A) },
    S#{ invoke => #{ action => Action } };

handle_start_el(occi, action, A, _Pos, S)  ->
    A0 = #{ term => attr("term", A), 
	    scheme => attr("scheme", A) },
    A1 = case attr("title", A, undefined) of
	     undefined -> A0;
	     Title -> A0#{ title => Title }
	 end,
    S#{ action => A1 };

handle_start_el(occi, parent, A, _Pos, #{ kind := Kind }=S) ->
    S#{ kind := Kind#{ parent => { attr("scheme", A), attr("term", A) } } };

%% action invocation attribute instance
handle_start_el(occi, attribute, A, _Pos, #{ invoke := Invoke,
					     parents := [ {occi, action}, document ] }=S) ->
    Attrs = maps:put(attr("name", A), attr("value", A), maps:get(attributes, Invoke, #{})),
    S#{ invoke := Invoke#{ attributes => Attrs } };

%% attribute spec
handle_start_el(occi, attribute, A, _Pos, #{ ns := NS, parents := [ {occi, Cls} | _ ] }=S) 
  when Cls =:= kind; Cls =:= mixin; Cls =:= action ->
    Name = attr("name", A),
    Spec = #{ type => type_(attr("type", A, undefined), NS),
	      title => attr("title", A, <<>>),
	      required => case attr("use", A, <<"optional">>) of
			      <<"optional">> -> false;
			      <<"required">> -> true
			  end,
	      mutable => case attr("immutable", A, <<"false">>) of
			     <<"true">> -> false;
			     <<"false">> -> true
			 end,
	      default => attr("default", A, undefined),
	      description => attr("description", A, <<>>) },
    S#{ attribute => { Name, Spec } };

%% entity attribute instance
handle_start_el(occi, attribute, A, _Pos, #{ resource := Res, parents := [ {occi, resource} | _ ] }=S) ->
    S#{ resource := update_entity(attr("name", A), attr("value", A), Res) };

handle_start_el(occi, attribute, A, _Pos, #{ link := Link, parents := [ {occi, link} | _ ] }=S) ->
    S#{ link := update_entity(attr("name", A), attr("value", A), Link) };

handle_start_el(xsd, restriction, A, _Pos, S) ->
    case attr("base", A) of
	<<"xs:string">> -> 
	    S;
	BaseType -> 
	    throw({invalid_attribute_type, BaseType})
    end;

handle_start_el(xsd, enumeration, A, _Pos, S) ->
    Value = binary_to_atom(attr("value", A), utf8),
    S#{ enum => [ Value | maps:get(enum, S, []) ] };

handle_start_el(Ns, El, _A, _Pos, S) ->
    Error = iolist_to_binary(io_lib:format("Invalid start tag: ~s:~s", [Ns, El])),
    ?debug("XML Parser State: ~p", [S]),
    throw({xml, Error}).


handle_end_el(occi, collection, _, #{ collection := Coll }=S) ->
    S#{ doc => Coll };

%% extension can only be a root node
handle_end_el(occi, extension, _, #{ extension := Ext }=S) ->
    S#{ doc => Ext };

handle_end_el(occi, import, _, S) ->
    S;

handle_end_el(occi, kind, _, #{ kind := Kind, extension := Ext, 
				parents := [ {occi, extension } | _ ] }=S) ->
    S1 = maps:remove(kind, S),
    S1#{ extension := Ext#{ kinds => [ Kind | maps:get(kinds, Ext, []) ] } };

handle_end_el(occi, kind, _, S) ->
    S;

handle_end_el(occi, mixin, _, #{ mixin := Mixin, extension := Ext,
				 parents := [ {occi, extension} | _ ] }=S) ->
    S1 = maps:remove(mixin, S),
    S1#{ extension := Ext#{ mixins => [ Mixin | maps:get(mixins, Ext, []) ] } };

%% mixin is a root node
handle_end_el(occi, mixin, _, #{ mixin := Mixin, parents := [ document ] }=S) ->
    S#{ doc => Mixin };

handle_end_el(occi, mixin, _, S) ->
    S;

handle_end_el(occi, resource, _, #{ resource := Res, collection := Coll,
				    parents := [ {occi, collection} | _ ] }=S) ->
    S1 = maps:remove(resource, S),
    S1#{ collection := Coll#{ resources => [ Res | maps:get(resources, Coll, []) ] } };

handle_end_el(occi, resource, _, #{ resource := Res, parents := [ document ] }=S) ->
    S#{ doc => Res };

handle_end_el(occi, link, _, #{ link := Link, collection := Coll,
				parents := [ {occi, collection} | _ ] }=S) ->
    S1 = maps:remove(link, S),
    S1#{ collection := Coll#{ links => [ Link | maps:get(links, Coll, []) ] } };

handle_end_el(occi, link, _, #{ link := Link, parents := [ document ] }=S) ->
    S#{ doc => Link };

handle_end_el(occi, link, _, #{ link := Link, resource := Res,
				parents := [ {occi, resource} | _ ] }=S) ->
    S1 = maps:remove(link, S),
    S1#{ resource := Res#{ links => [ Link | maps:get(links, Res, []) ] } };

handle_end_el(occi, depends, _, S) ->
    S;

handle_end_el(occi, applies, _, S) ->
    S;

handle_end_el(occi, action, _, #{ invoke := Invoke, parents := [ document ] }=S) ->
    S#{ doc => Invoke };

handle_end_el(occi, action, _, #{ action := Action, kind := Kind,
				  parents := [ {occi, kind} | _ ]}=S) ->
    S1 = maps:remove(action, S),
    S1#{ kind := Kind#{ actions => [ Action | maps:get(actions, Kind, []) ] } };

handle_end_el(occi, action, _, #{ action := Action, mixin := Mixin,
				  parents := [ {occi, mixin} | _ ]}=S) ->
    S1 = maps:remove(action, S),
    S1#{ mixin := Mixin#{ actions => [ Action | maps:get(actions, Mixin, []) ] } };

handle_end_el(occi, parent, _, S) ->
    S;

handle_end_el(occi, attribute, _, #{ attribute := { Name, Spec }, action := Action,
				     parents := [ {occi, action}, {occi, Cls} | _ ] }=S) 
  when kind =:= Cls; mixin =:= mixin ->
    Attrs = maps:put(Name, Spec, maps:get(attributes, Action, #{})),
    S1 = maps:remove(attribute, S),
    S1#{ action := Action#{ attributes => Attrs } };

handle_end_el(occi, attribute, _, #{ attribute := { Name, Spec }, kind := Kind,
				     parents := [ {occi, kind} | _ ] }=S) ->
    Attrs = maps:put(Name, Spec, maps:get(attributes, Kind, #{})),
    S1 = maps:remove(attribute, S),
    S1#{ kind := Kind#{ attributes => Attrs } };

handle_end_el(occi, attribute, _, #{ attribute := { Name, Spec }, mixin := Mixin,
				     parents := [ {occi, mixin} | _ ] }=S) ->
    Attrs = maps:put(Name, Spec, maps:get(attributes, Mixin, #{})),
    S1 = maps:remove(attribute, S),
    S1#{ mixin := Mixin#{ attributes => Attrs } };

handle_end_el(occi, attribute, _, #{ parents := [ {occi, resource} | _ ] }=S) ->
    S;

handle_end_el(occi, attribute, _, #{ parents := [ {occi, link} | _ ] }=S) ->
    S;

handle_end_el(occi, attribute, _, #{ parents := [ {occi, action} | _ ] }=S) ->
    S;

handle_end_el(xsd, restriction, _, #{ attribute := {Name, Spec},
				      enum := Values }=S) ->
    S1 = maps:remove(enum, S),
    S1#{ attribute := {Name, Spec#{ type := {enum, Values} } } };

handle_end_el(xsd, enumeration, _, S) ->
    S;

handle_end_el(Ns, El, _, S) ->
    Error = iolist_to_binary(io_lib:format("Invalid end tag: ~s:~s", [Ns, El])),
    ?debug("XML Parser State: ~p", [S]),
    throw({xml, Error}).



update_entity(Name, Value, Entity) ->
    Attrs0 = maps:get(attributes, Entity, #{}),
    Entity#{ attributes => Attrs0#{ Name => Value } }.


attr(_Name, [], Default) ->
    Default;

attr(Name, [ {_URI, _Prefix, Name, Value} | _Attributes ], _Default) ->
    list_to_binary(Value);

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
    case binary:split(QN, [<<":">>]) of
	[Prefix, Name] ->
	    from_xml_type(binary_to_list(Prefix), Name, NS);
	Else ->
	    throw({invalid_type, Else})
    end.


from_xml_type(Prefix, <<"string">>, #{ ?xsd_uri := Prefix }) ->
    string;
from_xml_type(Prefix, <<"integer">>,  #{ ?xsd_uri := Prefix }) ->
    integer;
from_xml_type(Prefix, <<"float">>, #{ ?xsd_uri := Prefix }) ->
    float;
from_xml_type(Prefix, <<"anyURI">>, #{ ?xsd_uri := Prefix }) ->
    uri;
from_xml_type(Prefix, <<"kind">>, #{ ?occi_uri := Prefix }) ->
    kind;
from_xml_type(Prefix, <<"resource">>, #{ ?occi_uri := Prefix }) ->
    resource;
from_xml_type(Prefix, Name, _) ->
    throw({invalid_type, << (list_to_binary(Prefix))/binary, Name >>}).


val_start_el(occi, collection,  document) ->              ok;
val_start_el(occi, extension,   document) ->              ok;
val_start_el(occi, import,      {occi, extension}) ->     ok;
val_start_el(occi, kind,        {occi, extension}) ->     ok;
val_start_el(occi, kind,        {occi, resource}) ->      ok;
val_start_el(occi, kind,        {occi, link}) ->          ok;
val_start_el(occi, mixin,       {occi, extension}) ->     ok;
val_start_el(occi, mixin,       {occi, resource}) ->      ok;
val_start_el(occi, mixin,       {occi, link}) ->          ok;
val_start_el(occi, mixin,       document) ->              ok;
val_start_el(occi, resource,    document) ->              ok;
val_start_el(occi, resource,    {occi, collection}) ->    ok;
val_start_el(occi, link,        document) ->              ok;
val_start_el(occi, link,        {occi, collection}) ->    ok;
val_start_el(occi, link,        {occi, resource}) ->      ok;
val_start_el(occi, depends,     {occi, mixin}) ->         ok;
val_start_el(occi, applies,     {occi, mixin}) ->         ok;
val_start_el(occi, action,      document) ->              ok;
val_start_el(occi, action,      {occi, kind}) ->          ok;
val_start_el(occi, action,      {occi, mixin}) ->         ok;
val_start_el(occi, parent,      {occi, kind}) ->          ok;
val_start_el(occi, attribute,   {occi, kind}) ->          ok;
val_start_el(occi, attribute,   {occi, mixin}) ->         ok;
val_start_el(occi, attribute,   {occi, action}) ->        ok;
val_start_el(occi, attribute,   {occi, resource}) ->      ok;
val_start_el(occi, attribute,   {occi, link}) ->          ok;
val_start_el(occi, attribute,   {occi, invoke}) ->        ok;
val_start_el(xsd,  restriction, {occi, attribute}) ->     ok;
val_start_el(xsd,  enumeration, {xsd, restriction}) ->    ok;
val_start_el(Ns, El, {ParentNs, ParentEl}) -> 
    Error = iolist_to_binary(io_lib:format("Invalid child: '~s:~s' -> '~s:~s'", [ParentNs, ParentEl, Ns, El])),
    throw({xml, Error}).


val_end_el(Ns, El, {Ns, El}) -> ok;
val_end_el(Ns, El, {OtherNs, OtherEl} ) -> 
    Error = iolist_to_binary(io_lib:format("Mismatch closing tag: '~s:~s' / '~s:~s'", [OtherNs, OtherEl, Ns, El])),
    throw({xml, Error}).


ns_to_atom(?occi_uri) ->  occi;
ns_to_atom(?xsd_uri) ->   xsd;
ns_to_atom(Ns) -> 
    Error = iolist_to_binary(io_lib:format("Unknown namespace: '~s'", [Ns])),
    throw({xml, Error}).


el_to_atom("collection") ->  collection;
el_to_atom("extension") ->   extension;
el_to_atom("import") ->      import;
el_to_atom("kind") ->        kind;
el_to_atom("mixin") ->       mixin;
el_to_atom("resource") ->    resource;
el_to_atom("link") ->        link;
el_to_atom("depends") ->     depends;
el_to_atom("applies") ->     applies;
el_to_atom("action") ->      action;
el_to_atom("parent") ->      parent;
el_to_atom("attribute") ->   attribute;
el_to_atom("restriction") -> restriction;
el_to_atom("enumeration") -> enumeration;
el_to_atom(El) -> 
    Error = iolist_to_binary(io_lib:format("Unknown Element: '~s'", [El])),
    throw({xml, Error}).

%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
