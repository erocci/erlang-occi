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
	 add_mixin/2]).

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

new(Id, KindId) ->
    #{id => Id,
      kind => KindId,
      mixins => []}.


-spec id(t()) -> uri:t().
id(E) ->
    maps:get(id, E).


-spec kind(t()) -> occi_kind:id().
kind(E) ->
    maps:get(kind, E).


-spec mixins(t()) -> [occi_mixin:id()].
mixins(E) ->
    maps:get(mixins, E).


-spec add_mixin(occi_mixin:id(), t()) -> t().
add_mixin(MixinId, E) ->
    Mixins = maps:get(mixins, E),
    E#{ mixins => [ MixinId | Mixins ]}.

%%%
%%% eunit
%%%
-ifdef(TEST).
new_test_() ->
    [
     ?_assertThrow({invalid_uri, ""}, new("", "")),
     ?_assertThrow({invalid_cid, ""}, new("http://example.org:8081/myentity0", ""))
    ].

-endif.
