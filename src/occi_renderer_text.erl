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

%% API
-export([render/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec render(T :: occi:t()) -> iolist().
render(T) ->
    Headers = to_headers(occi_type:type(T), T, #{}),
    maps:fold(fun (K, Values, Acc) ->
		      [K, ":", string:join(Values, ", "), "\n" | Acc]
	      end, [], Headers).


%%%
%%% Priv
%%%
to_headers(extension, Ext, Headers) ->
    H0 = lists:foldl(fun (K, Acc) ->
				   to_headers(kind, K, Acc)
			   end, Headers, occi_extension:kinds(Ext)),
    H1 = lists:foldl(fun (M, Acc) ->
			     to_headers(mixin, M, Acc)
		     end, H0, occi_extension:mixins(Ext)),
    CatActions = fun(Cat, Acc) ->
			  occi_category:actions(Cat) ++ Acc
		  end,
    Actions = lists:foldl(CatActions, 
			  lists:foldl(CatActions, [], occi_extension:kinds(Ext)), 
			  occi_extension:mixins(Ext)),
    lists:foldl(fun (A, Acc) ->
			     to_headers(action, A, Acc)
		     end, H1, Actions);

to_headers(kind, Kind, Headers) ->
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
    C2 = case attribute_list(occi_kind:attributes(Kind), []) of
	     [] -> C1;
	     Attributes -> [C1, "; attributes=\"", Attributes, "\""]
	 end,
    append_header("category", lists:flatten(C2), Headers);

to_headers(mixin, Mixin, Headers) ->
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
    C2 = case attribute_list(occi_mixin:attributes(Mixin), []) of
	     [] -> C1;
	     Attributes -> [C1, "; attributes=\"", Attributes, "\""]
	 end,
    append_header("category", lists:flatten(C2), Headers);

to_headers(action, Action, Headers) ->
    {Scheme, Term} = occi_action:id(Action),
    Cat = [Term, "; scheme=\"", Scheme, "\"; class=action"],
    C0 = case occi_action:title(Action) of
	     [] -> Cat;
	     Title -> [Cat, "; title=\"", Title, "\""]
	 end,
    C1 = case attribute_list(occi_action:attributes(Action), []) of
	     [] -> C0;
	     Attributes -> [C0, "; attributes=\"", Attributes, "\""]
	 end,
    append_header("category", lists:flatten(C1), Headers).
			 

attribute_list([], Acc) ->
    string:join(lists:reverse(Acc), " ");

attribute_list([ Attr | Tail ], Acc) ->
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
    attribute_list(Tail, [ lists:flatten(Def1) | Acc ]).


append_header(Name, Value, Headers) ->
    case maps:get(Name, Headers, undefined) of
	undefined -> Headers#{ Name => [Value]};
	Values -> Headers#{ Name := Values ++ [Value]}
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
