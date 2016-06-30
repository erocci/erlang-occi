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
%%% Created : 17 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_json).

-include("occi_log.hrl").

%% API
-export([render/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec render(T :: occi:t(), Ctx :: occi_uri:t()) -> iolist().
render(T, Ctx) ->
    Json = r_type(occi_type:type(T), T, Ctx),
    jsx:encode(Json, [space, {indent, 2}]).

%%%
%%% Priv
%%%
r_type(categories, Categories, Ctx) ->
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
    M0 = #{ kinds => lists:map(fun (Kind) -> 
				       r_kind(Kind, Ctx)
			       end, Kinds) },
    M1 = case Mixins of
	     [] ->
		 M0;
	     _ ->
		 M0#{ mixins => lists:map(fun (Mixin) ->
						  r_mixin(Mixin, Ctx)
					  end, Mixins) }
	 end,
    Actions = lists:foldl(fun (Category, Acc) ->
				  occi_category:actions(Category) ++ Acc
			  end, [], Kinds ++ Mixins),
    case Actions of
	[] ->
	    M1;
	_ ->
	    M1#{ actions => lists:map(fun (Action) ->
					      r_action(Action)
				      end, Actions) }
    end;

r_type(collection, Coll, Ctx) ->
    SplitFun = fun ({Location, undefined}, Acc) ->
		       Entity = #{ location => occi_uri:to_string(Location, Ctx) },
		       Acc#{ entities => [ Entity | maps:get(entities, Acc, []) ] };
		   ({_, E}, Acc) ->
		       case occi_type:type(E) of
			   resource ->
			       Resource = r_type(resource, E, Ctx),
			       Acc#{ resources => [ Resource | maps:get(resources, Acc, []) ] };
			   link ->
			       Link = r_link(E, Ctx),
			       Acc#{ links => [ Link | maps:get(links, Acc, []) ] }
		       end
	       end,
    ordsets:fold(SplitFun, #{}, occi_collection:elements(Coll));

r_type(resource, R, Ctx) ->
    M = r_entity(R, Ctx),
    M1 = case occi_resource:links(R) of
	     [] -> M;
	     Links -> M#{ links => r_links(Links, [], Ctx) }
	 end,
    case occi_resource:get(<<"occi.core.summary">>, R) of
	undefined -> M1;
	Summary -> M1#{ summary => r_string(Summary) }
    end;

r_type(link, L, Ctx) ->
    r_link(L, Ctx).  


r_kind(Kind, Ctx) ->
    M = r_category(Kind, Ctx),
    case occi_kind:parent(Kind) of
	undefined ->
	    M;
	Parent ->
	    M#{ parent => r_type_id(Parent) }
    end.


r_mixin(Mixin, Ctx) ->
    M = r_category(Mixin, Ctx),
    M1 = case occi_mixin:depends(Mixin) of
	     [] ->
		 M;
	     Depends ->
		 M#{ depends => [ r_type_id(Dep) || Dep <- Depends ] }
	 end,
    case occi_mixin:applies(Mixin) of
	[] ->
	    M1;
	Applies ->
	    M1#{ applies => [ r_type_id(Apply) || Apply <- Applies ] }
    end.


r_action(Action) ->
    {Scheme, Term} = occi_action:id(Action),
    M = #{ term => Term, scheme => Scheme },
    M0 = case occi_action:title(Action) of
	     <<>> -> M;
	     Title -> M#{ title => Title }
	 end,
    case occi_action:attributes(Action) of
	[] ->
	    M0;
	Attributes ->
	    Defs = lists:foldl(fun (Def, Acc) ->
				       Name = occi_attribute:name(Def),
				       Acc#{ Name => r_attr_def(Def) }
			       end, #{}, Attributes),
	    M0#{ attributes => Defs }
    end.


r_category(Category, Ctx) ->
    {Scheme, Term} = occi_category:id(Category),
    M = #{ term => Term, scheme => Scheme },
    M0 = case occi_category:title(Category) of
	     <<>> -> M;
	     Title -> M#{ title => Title }
	 end,
    M1 = case occi_category:attributes(Category) of
	     [] ->
		 M0;
	     Attributes ->
		 Defs = lists:foldl(fun (Def, Acc) ->
					    Name = occi_attribute:name(Def),
					    Acc#{ Name => r_attr_def(Def) }
				    end, #{}, Attributes),
		 M0#{ attributes => Defs }
	 end,
    M2 = case occi_category:actions(Category) of
	     [] ->
		 M1;
	     Actions ->
		 M1#{ actions => [ r_type_id(occi_action:id(Action)) || Action <- Actions ] }
	 end,
    case occi_category:location(Category) of
	undefined ->
	    M2;
	Location ->
	    M2#{ location => occi_uri:to_string(Location, Ctx) }
    end.


r_attr_def(Def) ->
    M = #{ mutable => occi_attribute:mutable(Def),
	   required => occi_attribute:required(Def),
	   type => r_attr_type(occi_attribute:type(Def))
	 },
    M0 = case occi_attribute:pattern(Def) of
	     undefined ->
		 r_attr_pattern(Def, M);
	     Pattern ->
		 M#{ pattern => Pattern }
	 end,
    M1 = case occi_attribute:default(Def) of
	     undefined ->
		 M0;
	     Default ->
		 M0#{ default => r_attribute_value(Default) }
	 end,
    case occi_attribute:description(Def) of
	<<>> ->
	    M1;
	Desc ->
	    M1#{ description => Desc }
    end.


r_attr_pattern(Def, Map) ->
    Pattern = #{
      <<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
      <<"type">> => r_attr_type(occi_attribute:type(Def))
     },
    Map#{ pattern => Pattern }.


r_attr_type({enum, _}) ->
    array;

r_attr_type(string) ->
    string;

r_attr_type(integer) ->
    number;

r_attr_type(float) ->
    number;

r_attr_type(boolean) ->
    boolean;

r_attr_type(uri) ->
    string;

r_attr_type(kind) ->
    string;

r_attr_type(resource) ->
    string.


r_link(L, Ctx) ->
    M = r_entity(L, Ctx),
    M#{ id => occi_resource:id(L),
	source => r_link_end(occi_link:get(<<"occi.core.source">>, L), 
			     occi_link:get(<<"occi.core.source.kind">>, L), Ctx),
	target => r_link_end(occi_link:get(<<"occi.core.target">>, L), 
			     occi_link:get(<<"occi.core.target.kind">>, L), Ctx) }.


r_entity(E, Ctx) ->
    M = #{ kind => r_type_id(occi_entity:kind(E)) },
    M1 = case occi_entity:mixins(E) of
	     [] -> M;
	     Mixins -> M#{ mixins => [ r_type_id(Id) || Id <- Mixins ]}
	 end,
    Attrs = r_attributes(occi_entity:attributes(E)),
    M2 = case maps:size(Attrs) of
	     0 -> M1;
	     _ -> M1#{ attributes => Attrs }
	 end,
    M3 = case occi_entity:actions(E) of
	     [] -> 
		 M2;
	     Actions -> 
		 M2#{ actions => [ r_type_id(Action) || Action <- Actions ] }
	 end,
    M4 = case occi_entity:id(E) of
	     undefined -> M3;
	     Id -> M3#{ id => Id }
	 end,
    M5 = M4#{ location => occi_uri:to_string(occi_entity:location(E), Ctx) },
    case occi_link:get(<<"occi.core.title">>, E) of
	undefined -> M5;
	Title -> M5#{ title => r_string(Title) }
    end.


r_type_id({Scheme, Term}) ->
    << Scheme/binary, Term/binary >>.


r_links([], Acc, _) ->
    lists:reverse(Acc);

r_links([ Link | Tail ], Acc, Ctx) ->
    r_links(Tail, [ r_link(Link, Ctx) | Acc ], Ctx).

r_link_end(Location, undefined, Ctx) ->
    #{ location => r_string(occi_uri:to_string(Location, Ctx)) };

r_link_end(Location, Kind, Ctx) ->
    #{ location => occi_uri:to_string(Location, Ctx),
       kind => r_type_id(Kind) }.


r_attributes(Attributes) ->
    maps:fold(fun (_, undefined, Acc) ->	       Acc;
		  (<<"occi.core.id">>, _, Acc) ->	       Acc;
		  (<<"occi.core.summary">>, _, Acc) ->     Acc;
		  (<<"occi.core.title">>, _, Acc) ->       Acc;
		  (<<"occi.core.source">>, _, Acc) ->      Acc;
		  (<<"occi.core.source.kind">>, _, Acc) -> Acc;
		  (<<"occi.core.target">>, _, Acc) ->      Acc;
		  (<<"occi.core.target.kind">>, _, Acc) -> Acc;
		  (K, V, Acc) ->                       
		      Acc#{ iolist_to_binary(K) => r_attribute_value(V) }
	      end, #{}, Attributes).


r_attribute_value(V) when is_list(V) ->
    iolist_to_binary(V);

r_attribute_value(V) ->
    V.

r_string(S) ->
    iolist_to_binary(S).

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
