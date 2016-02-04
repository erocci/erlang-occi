%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_entity).

-export([new/2,
	 id/1,
	 kind/1,
	 mixins/1,
	 add_mixin/2,
	 title/1,
	 title/2]).

-type t() :: #{}.
-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new(uri:t() | string() | binary(), occi_category:id() | string() | binary()) -> t().
new(IdStr, KindId) when is_list(IdStr); is_binary(IdStr) ->
    try uri:from_string(IdStr) of
	Id -> new(Id, KindId)
    catch
	error:{badmatch, _} ->
	    throw({invalid_uri, IdStr})
    end;

new(Id, KindId) when is_list(KindId); is_binary(KindId) ->
    new(Id, occi_category:parse_id(KindId));

new(Id, {Scheme, Term}) ->
    #{id => Id,
      kind => {Scheme, Term},
      mixins => [],
      title => "",
      attributes => #{}}.


-spec id(t()) -> uri:t().
id(E) ->
    maps:get(id, E).


-spec kind(t()) -> occi_kind:id().
kind(E) ->
    maps:get(kind, E).


-spec mixins(t()) -> [occi_mixin:id()].
mixins(E) ->
    maps:get(mixins, E).


-spec add_mixin(occi_category:id() | string() | binary(), t()) -> t().
add_mixin(MixinId, E) when is_list(MixinId); is_binary(MixinId) ->
    add_mixin(occi_category:parse_id(MixinId), E);

add_mixin(MixinId, E) ->
    Mixins = maps:get(mixins, E),
    E#{ mixins := [ MixinId | Mixins ]}.


-spec title(t()) -> string().
title(E) ->
    maps:get(title, E).


-spec title(string() | binary(), t()) -> t().
title(Title, E) when is_binary(Title) ->
    title(binary_to_list(Title), E);

title(Title, E) when is_list(Title) ->
    E#{ title := Title }.

%%%
%%% eunit
%%%
-ifdef(TEST).
new_test_() ->
    [
     ?_assertThrow({invalid_uri, ""}, new("", "")),
     ?_assertThrow({invalid_cid, ""}, new("http://example.org:8081/myentity0", "")),
     ?_assertMatch(#{ id := {uri, <<"http">>, <<>>, <<"example.org">>, 8081, <<"/myentity0">>, [], <<>>, <<"http://example.org:8081/myentity0">>}, kind := {"http://example.org/occi#", "type"} }, new("http://example.org:8081/myentity0", "http://example.org/occi#type"))
    ].

-endif.
