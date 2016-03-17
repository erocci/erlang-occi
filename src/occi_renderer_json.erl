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
-spec render(T :: occi:t(), Ctx :: uri:t()) -> iolist().
render(T, Ctx) ->
    Json = r_type(occi_type:type(T), T, Ctx),
    ?debug("json=~p", [Json]),
    jsx:encode(Json, [space, {indent, 2}]).

%%%
%%% Priv
%%%
r_type(resource, R, Ctx) ->
    M = r_entity(R, Ctx),
    M1 = case occi_resource:links(R) of
	     [] -> M;
	     Links -> M#{ links => r_links(Links, [], Ctx) }
	 end,
    case occi_resource:get("occi.core.summary", R) of
	undefined -> M1;
	Summary -> M1#{ summary => r_string(Summary) }
    end.


r_link(L, Ctx) ->
    M = r_entity(L, Ctx),
    M#{ id => r_string(occi_utils:ctx(occi_resource:id(L), Ctx)),
	source => r_link_end(occi_link:get("occi.core.source", L), 
			     occi_link:get("occi.core.source.kind", L), Ctx),
	target => r_link_end(occi_link:get("occi.core.target", L), 
			     occi_link:get("occi.core.target.kind", L), Ctx) }.


r_entity(E, Ctx) ->
    M = #{ kind => r_type_id(occi_entity:kind(E)) },
    M1 = case occi_resource:mixins(E) of
	     [] -> M;
	     Mixins -> M#{ mixins => [ r_type_id(Id) || Id <- Mixins ]}
	 end,
    Attrs = r_attributes(occi_entity:attributes(E)),
    M2 = case maps:size(Attrs) of
	     0 -> M1;
	     _ -> M1#{ attributes => Attrs }
	 end,
    M3 = M2#{ id => r_string(occi_utils:ctx(occi_entity:id(E), Ctx)) },
    case occi_link:get("occi.core.title", E) of
	undefined -> M3;
	Title -> M3#{ title => r_string(Title) }
    end.


r_type_id({Scheme, Term}) ->
    iolist_to_binary(Scheme ++ Term).


r_links([], Acc, _) ->
    lists:reverse(Acc);

r_links([ Link | Tail ], Acc, Ctx) ->
    r_links(Tail, [ r_link(Link, Ctx) | Acc ], Ctx).

r_link_end(Location, undefined, Ctx) ->
    #{ location => r_string(occi_utils:ctx(Location, Ctx)) };

r_link_end(Location, Kind, Ctx) ->
    #{ location => r_string(occi_utils:ctx(Location, Ctx)),
       kind => r_type_id(Kind) }.


r_attributes(Attributes) ->
    maps:fold(fun (_, undefined, Acc) ->	       Acc;
		  ("occi.core.id", _, Acc) ->	       Acc;
		  ("occi.core.summary", _, Acc) ->     Acc;
		  ("occi.core.title", _, Acc) ->       Acc;
		  ("occi.core.source", _, Acc) ->      Acc;
		  ("occi.core.source.kind", _, Acc) -> Acc;
		  ("occi.core.target", _, Acc) ->      Acc;
		  ("occi.core.target.kind", _, Acc) -> Acc;
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
