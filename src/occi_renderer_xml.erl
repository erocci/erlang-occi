%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_renderer_xml).

-include("occi_uri.hrl").
-include("occi_xml.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-export([render/2,
	 to_xml/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec render(T :: occi_type:t(), Ctx :: occi_ctx:t()) -> iolist().
render(T, Ctx) ->
    to_document(occi_type:type(T), T, Ctx).
    

-spec to_xml(TypeName :: occi_type:name(), T :: occi_type:t(), Ctx :: uri:t()) -> tuple().
to_xml(categories, Categories, Ctx) ->
    {Kinds, Mixins} = lists:foldl(fun (Cat, {KindAcc, MixinsAcc}) ->
					  case occi_category:class(Cat) of
					      kind ->
						  { [ Cat | KindAcc ], MixinsAcc };
					      mixin ->
						  { KindAcc, [ Cat | MixinsAcc ]};
					      _ ->
						  { KindAcc, MixinsAcc }
					  end
				 end, {[], []}, Categories),
    C0 = lists:foldl(fun (Kind, Acc) ->
			     [ to_xml(kind, Kind, Ctx) | Acc ]
		     end, [], Kinds),
    C1 = lists:foldl(fun (Mixin, Acc) ->
			     [ to_xml(mixin, Mixin, Ctx) | Acc ]
		     end, C0, Mixins),
    {capabilities, [], lists:reverse(C1)};

to_xml(collection, Coll, Ctx) ->
    A = case occi_collection:id(Coll) of
	    Id when ?is_uri(Id) ->
		[{id, occi_uri:to_string(Id)}];
	    {Scheme, Term} ->
		[{scheme, Scheme}, {term, Term}];
	    undefined ->
		[]
	end,
    C = sets:fold(fun ({Id, undefined}, Acc) ->
			  [ {location, [{href, occi_uri:to_string(Id, Ctx)}], []} | Acc ];
		      ({_, E}, Acc) ->
			  [ to_xml(occi_type:type(E), E, Ctx) | Acc ]
		  end, [], occi_collection:elements(Coll)),
    {collection, A, lists:reverse(C)};


to_xml(extension, T, Ctx) ->
    A0 = [{scheme, occi_extension:scheme(T)}],
    A1 = case occi_extension:name(T) of
	     [] -> A0;
	     Name -> [{name, Name} | A0]
	 end,
    C0 = lists:foldl(fun (I, Acc) ->
			     [{import, [{scheme, I}], []} | Acc]
		     end, [], occi_extension:imports(T)),
    C1 = lists:foldl(fun (K, Acc) ->
			     [to_xml(kind, K, Ctx) | Acc]
		     end, C0, occi_extension:kinds(T)),
    C2 = lists:foldl(fun (M, Acc) ->
			     [to_xml(mixin, M, Ctx) | Acc]
		     end, C1, occi_extension:mixins(T)),
    {extension, lists:reverse(A1), lists:reverse(C2)};

to_xml(kind, K, Ctx) ->
    {Scheme, Term} = occi_kind:id(K),
    A = [{scheme, Scheme}, {term, Term}],
    A0 = case occi_kind:title(K) of
	     [] -> A;
	     Title -> [{title, Title} | A]
	 end,
    A1 = case occi_kind:location(K) of
	     undefined -> A0;
	     Location -> [{location, occi_uri:to_string(Location, Ctx)} | A0]
	 end,
    C0 = case occi_kind:parent(K) of
	     undefined -> [];
	     ParentId -> [category_id(parent, ParentId)]
	 end,
    C1 = lists:foldl(fun (Attr, Acc) ->
			   [ to_xml(attribute_def, Attr, Ctx) | Acc ]
		   end, C0, occi_kind:attributes(K)),
    C2 = lists:foldl(fun (Action, Acc) ->
			     [ to_xml(action, Action, Ctx) | Acc ]
		     end, C1, occi_kind:actions(K)),
    {kind, lists:reverse(A1), lists:reverse(C2)};

to_xml(mixin, M, Ctx) ->
    {Scheme, Term} = occi_mixin:id(M),
    A = [{scheme, Scheme}, {term, Term}],
    A0 = case occi_mixin:title(M) of
	     [] -> A;
	     Title -> [{title, Title} | A]
	 end,
    A1 = case occi_mixin:location(M) of
	     undefined -> A0;
	     Location -> [{location, occi_uri:to_string(Location, Ctx)} | A0]
	 end,
    C = lists:foldl(fun (DepId, Acc) ->
			    [ category_id(depends, DepId) | Acc ]
		    end, [], occi_mixin:depends(M)),
    C0 = lists:foldl(fun (ApplyId, Acc) ->
			     [ category_id(applies, ApplyId) | Acc ]
		     end, C, occi_mixin:applies(M)),
    C1 = lists:foldl(fun (Attr, Acc) ->
			   [ to_xml(attribute, Attr, Ctx) | Acc ]
		   end, C0, occi_mixin:attributes(M)),
    C2 = lists:foldl(fun (Action, Acc) ->
			     [ to_xml(action, Action, Ctx) | Acc ]
		     end, C1, occi_mixin:actions(M)),
    {mixin, lists:reverse(A1), lists:reverse(C2)};

to_xml(attribute_def, Def, _Ctx) ->
    A = [ {name, occi_attribute:name(Def)} ],
    C = [],
    {A0, C0} = case occi_attribute:type(Def) of
		   {enum, Enum} ->
		       {A, [ type_def_enum(Enum) | C ]};
		   Type ->
		       {[ {type, type_def(Type)} | A ], C}
	       end,
    {attribute, lists:reverse(A0), lists:reverse(C0)};

to_xml(action, Action, Ctx) ->
    {Scheme, Term} = occi_action:id(Action),
    A = [{scheme, Scheme}, {term, Term}],
    A0 = case occi_action:title(Action) of
	     [] -> A;
	     Title -> [{title, Title} | A]
	 end,
    C = lists:foldl(fun (Attr, Acc) ->
			    [ to_xml(attribute, Attr, Ctx) | Acc ]
		    end, [], occi_action:attributes(Action)),
    {action, lists:reverse(A0), lists:reverse(C)};

to_xml(attribute, Attr, _Ctx) ->
    A = [{name, occi_attribute:name(Attr)}],
    C = [],
    {A0, C0} = case occi_attribute:type(Attr) of
		   string ->   {[{type, "xs:string"} | A], C};
		   integer ->  {[{type, "xs:integer"} | A], C};
		   float ->    {[{type, "xs:float"} | A], C};
		   {enum, Values} ->
		       Restriction = {'xs:restriction', [{base, 'xs:string'}], 
				      lists:map(fun (V) ->
							{'xs:enumeration', [{value, V}], []}
						end, Values)}, 
		       {A, [Restriction, C]}
	       end,
    A1 = case occi_attribute:title(Attr) of
	     <<>> -> A0;
	     Title -> [{title, Title} | A0]
	 end,
    A2 = case occi_attribute:default(Attr) of
	     undefined -> A1;
	     Default -> [{default, Default} | A1]
	 end,
    A3 = case occi_attribute:mutable(Attr) of
	     true -> A2;
	     false -> [{immutable, "true"} | A2]
	 end,
    A4 = case occi_attribute:required(Attr) of
	     true -> [{use, "required"} | A3];
	     false -> A3
	 end,
    A5 = case occi_attribute:description(Attr) of
	     <<>> -> A4;
	     Desc -> [{description, Desc} | A4]
	 end,
    {attribute, lists:reverse(A5), lists:reverse(C0)};

to_xml(resource, R, Ctx) ->
    Id = occi_resource:id(R),
    A = [{id, occi_uri:to_string(Id, Ctx)}],
    A1 = case occi_resource:get(<<"occi.core.title">>, R) of
	     undefined -> A;
	     Title -> [{title, Title} | A]
	 end,
    C = [ category_id(kind, occi_resource:kind(R)) ],
    C0 = lists:foldl(fun (MixinId, Acc) ->
			     [ category_id(mixin, MixinId) | Acc ]
		     end, C, occi_resource:mixins(R)),
    C1 = maps:fold(fun (<<"occi.core.title">>, _V, Acc) ->
			   Acc;
		       (_K, undefined, Acc) ->
			   Acc;
		       (K, V, Acc) ->
			   [ {attribute, [{name, K}, {value, attr_value(V)}], []} | Acc ]
		   end, C0, occi_resource:attributes(R)),
    C2 = lists:foldl(fun (L, Acc) ->
			     [ to_xml(link, L, Ctx) | Acc ]
		     end, C1, occi_resource:links(R)),
    C3 = lists:foldl(fun ({Scheme, Term}, Acc) ->
			     [ {action, [{scheme, Scheme}, {term, Term}], []} | Acc ]
		     end, C2, occi_resource:actions(R)),
    {resource, lists:reverse(A1), lists:reverse(C3)};

to_xml(link, L, Ctx) ->
    Id = occi_link:id(L),
    A = [
	 {target, occi_uri:to_string(occi_link:get(<<"occi.core.target">>, L), Ctx)},
	 {source, occi_uri:to_string(occi_link:get(<<"occi.core.source">>, L), Ctx)},
	 {id, occi_uri:to_string(Id, Ctx)}
	],
    A1 = case occi_link:get(<<"occi.core.title">>, L) of
	     undefined -> A;
	     Title -> [{title, Title} | A]
	 end,
    C = [ category_id(kind, occi_link:kind(L)) ],
    C0 = lists:foldl(fun (MixinId, Acc) ->
			     [ category_id(mixin, MixinId) | Acc ]
		     end, C, occi_link:mixins(L)),
    C1 = lists:foldl(fun ({Scheme, Term}, Acc) ->
			     [ {action, [{scheme, Scheme}, {term, Term}], []} | Acc ]
		     end, C0, occi_link:actions(L)),
    C2 = maps:fold(fun (<<"occi.core.title">>, _V, Acc) -> Acc;
		       (<<"occi.core.source">>, _V, Acc) -> Acc;
		       (<<"occi.core.target">>, _V, Acc) -> Acc;
		       (_K, undefined, Acc) -> Acc;
		       (K, V, Acc) ->
			   [ {attribute, [{name, K}, {value, attr_value(V)}], []} | Acc ]
		   end, C1, occi_resource:attributes(L)),
    {link, lists:reverse(A1), lists:reverse(C2)}.


%%%
%%% Priv
%%%
to_document(Type, T, Ctx) ->
    {Name, Attrs, Children} = to_xml(Type, T, Ctx),
    Ns = [{xmlns, ?occi_uri}, {'xmlns:xs', ?xsd_uri}],
    xmerl:export_simple([{Name, Attrs ++ Ns, Children}], occi_xml, []).


category_id(Name, {Scheme, Term}) ->
    {Name, [{scheme, Scheme}, {term, Term}], []}.


attr_value(V) when is_float(V) ->
    io_lib:format("~p", [V]);

attr_value({Scheme, Term}) when is_binary(Scheme), is_binary(Term) ->
    [ binary_to_list(Scheme) ++ binary_to_list(Term) ];

attr_value(V) -> 
    V.


type_def_enum(Values) ->
    {'xs:restriction', [{base, 'xs:string'}], type_def_enum_values(Values, [])}.

type_def_enum_values([], Acc) ->
    lists:reverse(Acc);

type_def_enum_values([ Value | Tail ], Acc) ->
    Enum = {'xs:enumeration', [{value, Value}], []},
    [ Enum | type_def_enum_values(Tail, Acc) ].


type_def(string) -> 'xs:string';
type_def(integer) -> 'xs:integer';
type_def(float) -> 'xs:float';
type_def(uri) -> 'xs:anyURI';
type_def(kind) -> 'kind';
type_def(resource) -> 'resource'.
