%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2013-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_text).

-include("occi_log.hrl").

%% API
-export([render/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec render(T :: occi:t(), Ctx :: uri:t()) -> iolist().
render(T, Ctx) ->
    Headers = to_headers(occi_type:type(T), T, orddict:new(), Ctx),
    orddict:fold(fun (K, Values, Acc) ->
			 [ Acc, "\n", K, ": ", string:join(Values, ", ") ]
		 end, [], Headers).


%%%
%%% Priv
%%%
to_headers(categories, Categories, Headers, Ctx) ->
    {Kinds, Mixins} = lists:foldl(fun (Cat, {KindAcc, MixinsAcc}) ->
					 case occi_category:class(Cat) of
					     kind ->
						 { [ Cat | KindAcc ], MixinsAcc };
					     mixin ->
						 { KindAcc, [ Cat | MixinsAcc ]}
					 end
				 end, {[], []}, Categories),
    H0 = lists:foldl(fun (Kind, Acc) ->
			     to_headers(kind, Kind, Acc, Ctx)
		     end, Headers, Kinds),
    H1 = lists:foldl(fun (Mixin, Acc) ->
			     to_headers(mixin, Mixin, Acc, Ctx)
		     end, H0, Mixins),
    CatActions = fun(Cat, Acc) ->
			  occi_category:actions(Cat) ++ Acc
		  end,
    Actions = lists:foldl(CatActions, [], Categories),
    lists:foldl(fun (A, Acc) ->
			     to_headers(action, A, Acc, Ctx)
		     end, H1, Actions);

to_headers(extension, Ext, Headers, Ctx) ->
    Categories = occi_extension:kinds(Ext) ++ occi_extension:mixins(Ext),
    to_headers(categories, Categories, Headers, Ctx);

to_headers(kind, Kind, Headers, Ctx) ->
    {Scheme, Term} = occi_kind:id(Kind),
    Cat = [Term, "; scheme=\"", Scheme, "\"; class=kind"],
    C0 = case occi_kind:title(Kind) of
	     [] -> Cat;
	     Title -> [Cat, "; title=\"", Title, "\""]
	 end,
    C1 = case occi_kind:parent(Kind) of
	     undefined -> C0;
	     {ParentScheme, ParentTerm} -> [C0, "; rel=\"", ParentScheme, ParentTerm, "\""]
	 end,
    C2 = case occi_kind:location(Kind) of
	     undefined -> C1;
	     Location -> [C1, "; location=\"", occi_uri:to_string(Location, Ctx), "\""]
	 end,
    C3 = case r_attribute_defs(occi_kind:attributes(Kind), []) of
	     [] -> C2;
	     Attributes -> [C2, "; attributes=\"", Attributes, "\""]
	 end,
    append("category", lists:flatten(C3), Headers);

to_headers(mixin, Mixin, Headers, Ctx) ->
    {Scheme, Term} = occi_mixin:id(Mixin),
    Cat = [Term, "; scheme=\"", Scheme, "\"; class=mixin"],
    C0 = case occi_mixin:title(Mixin) of
	     [] -> Cat;
	     Title -> [Cat, "; title=\"", Title, "\""]
	 end,
    C1 = case occi_mixin:depends(Mixin) of
	     [] -> C0;
	     [ {RelScheme, RelTerm} | _ ]-> [C0, "; rel=\"", RelScheme, RelTerm, "\""]
	 end,
    C2 = case occi_mixin:location(Mixin) of
	     undefined -> C1;
	     Location -> [C1, "; location=\"", occi_uri:to_string(Location, Ctx), "\""]
	 end,
    C3 = case r_attribute_defs(occi_mixin:attributes(Mixin), []) of
	     [] -> C2;
	     Attributes -> [C2, "; attributes=\"", Attributes, "\""]
	 end,
    append("category", lists:flatten(C3), Headers);

to_headers(action, Action, Headers, _Ctx) ->
    {Scheme, Term} = occi_action:id(Action),
    Cat = [Term, "; scheme=\"", Scheme, "\"; class=action"],
    C0 = case occi_action:title(Action) of
	     [] -> Cat;
	     Title -> [Cat, "; title=\"", Title, "\""]
	 end,
    C1 = case r_attribute_defs(occi_action:attributes(Action), []) of
	     [] -> C0;
	     Attributes -> [C0, "; attributes=\"", Attributes, "\""]
	 end,
    append("category", lists:flatten(C1), Headers);

to_headers(resource, Resource, Headers, Ctx) ->
    Url = occi_uri:to_string(occi_resource:id(Resource), Ctx),
    H0 = append("category", r_category_id(kind, occi_resource:kind(Resource)), Headers),
    H1 = lists:foldl(fun (MixinId, Acc) ->
			     append("category", r_category_id(mixin, MixinId), Acc)
		     end, H0, occi_resource:mixins(Resource)),
    H2 = lists:foldl(fun (Link, Acc) ->
			     append("link", r_resource_link(Link, Ctx), Acc)
		     end, H1, occi_resource:links(Resource)),
    H3 = lists:foldl(fun (Action, Acc) ->
			     append("link", r_action_link(Url, Action), Acc)
		     end, H2, occi_resource:actions(Resource)),
    H4 = maps:fold(fun (_, undefined, Acc) ->
			   Acc;
		       (K, V, Acc) ->
			   append("x-occi-attribute", r_attribute(K, V), Acc)
		   end, H3, occi_resource:attributes(Resource)),
    append("x-occi-attribute", r_attribute("occi.core.id", Url), H4).


r_attribute_defs([], Acc) ->
    string:join(Acc, " ");

r_attribute_defs([ Attr | Tail ], Acc) ->
    Def = occi_attribute:name(Attr),
    Props = case occi_attribute:mutable(Attr) of
		true -> [];
		false -> ["immutable"]
	    end,
    Props1 = case occi_attribute:required(Attr) of
		 true -> ["required" | Props];
		 false -> Props
	     end,
    Def1 = case Props1 of
	       [] -> Def;
	       _ -> [ Def, "{", string:join(Props1, " "), "}" ]
	   end,
    r_attribute_defs(Tail, [ lists:flatten(Def1) | Acc ]).


r_category_id(Class, {Scheme, Term}) ->
    [ Term, "; scheme=\"", Scheme, "\"; class=", atom_to_list(Class) ].


r_resource_link(Link, Ctx) ->
    Rel = case occi_link:get("occi.core.target.kind", Link) of
	      undefined ->
		  {"http://schemas.ogf.org/core/occi#", "entity"};
	      TypeId ->
		  TypeId
	  end,
    Categories = [ r_type_id(occi_link:kind(Link)), 
		   [ r_type_id(MixinId) || MixinId <- occi_link:mixins(Link) ]],
    L = [ 
	  "<", occi_uri:to_string(occi_link:get("occi.core.target", Link), Ctx), ">; rel=\"", 
	  r_type_id(Rel), "\"; self=\"", 
	  occi_uri:to_string(occi_link:id(Link), Ctx) ,
	  "\"; category=\"", Categories, "\""
	], 
    maps:fold(fun ("occi.core.id", _, Acc) ->
		      Acc;
		  ("occi.core.source", _, Acc) ->
		      Acc;
		  ("occi.core.source.kind", _, Acc) ->
		      Acc;
		  ("occi.core.target", _, Acc) ->
		      Acc;
		  ("occi.core.target.kind", _, Acc) ->
		      Acc;
		  (_, undefined, Acc) ->
		      Acc;
		  (K, V, Acc) ->
		      [ [ "; ", K, "=", r_attribute_value(V) ] | Acc ]
	      end, L, occi_link:attributes(Link)).


r_action_link(Id, {Scheme, Term}) ->
    [ "<", Id, "?action=", Term, ">; rel=\"", Scheme, Term, "\"" ].


r_attribute(K, V) ->
    [ K, "=", r_attribute_value(V) ].


r_type_id({Scheme, Term}) ->
    [ Scheme, Term ].


r_attribute_value(V) when is_atom(V) ->
    io_lib:format("\"~s\"", [V]);

r_attribute_value(V) when is_integer(V) ->
    io_lib:format("~b", [V]);

r_attribute_value(V) when is_float(V) ->
    io_lib:format("~g", [V]);

r_attribute_value(V) ->
    [ "\"", V, "\"" ].


append(Name, Value, Headers) ->
    case orddict:is_key(Name, Headers) of
	true ->
	    orddict:append_list(Name, [Value], Headers);
	false ->
	    orddict:store(Name, [Value], Headers)
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
