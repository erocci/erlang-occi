%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_renderer_xml).

-include("occi_xml.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-export([render/1,
	 to_xml/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec render(T :: occi:t()) -> iolist().
render(T) ->
    to_document(occi_type:type(T), T).
    

to_xml(extension, T) ->
    A0 = [{scheme, occi_extension:scheme(T)}],
    A1 = case occi_extension:name(T) of
	     [] -> A0;
	     Name -> [{name, Name} | A0]
	 end,
    C0 = lists:foldl(fun (I, Acc) ->
			     [{import, [{scheme, I}], []} | Acc]
		     end, [], occi_extension:imports(T)),
    C1 = lists:foldl(fun (K, Acc) ->
			     [to_xml(kind, K) | Acc]
		     end, C0, occi_extension:kinds(T)),
    C2 = lists:foldl(fun (M, Acc) ->
			     [to_xml(mixin, M) | Acc]
		     end, C1, occi_extension:mixins(T)),
    {extension, lists:reverse(A1), lists:reverse(C2)};

to_xml(kind, K) ->
    {Scheme, Term} = occi_kind:id(K),
    A = [{scheme, Scheme}, {term, Term}],
    A0 = case occi_kind:title(K) of
	     [] -> A;
	     Title -> [{title, Title} | A]
	 end,
    C = [],
    {kind, lists:reverse(A0), lists:reverse(C)};

to_xml(mixin, _M) ->
    {mixin, [], []}.


%%%
%%% Priv
%%%
to_document(Type, T) ->
    Ns = [{xmlns, ?occi_uri}, {'xmlns:xs', ?xsd_uri}],
    xmerl:export_simple([to_xml(Type, T)], xmerl_xml, Ns).


%%%
%%% eunit
%%%
-ifdef(TEST).
render_extension_test_() ->
    E = occi_extension:new("http://example.org"),
    [
     ?_assertMatch(["<?xml version=\"1.0\"?>" | _], render(E))
    ].

-endif.
