%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_resource).

-mixin([occi_entity]).

-export([new/2,
	 summary/1,
	 summary/2]).

-type t() :: occi_entity:t().
-export_type([t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new(uri:t() | string() | binary(), occi_category:id() | string() | binary()) -> t().
new(Id, KindId) ->
    Entity = occi_entity:new(Id, KindId),
    Entity#{ summary => ""}.


-spec summary(t()) -> string().
summary(E) ->
    maps:get(summary, E).


-spec summary(string() | binary(), t()) -> t().
summary(Summary, E) when is_binary(Summary) ->
    summary(binary_to_list(Summary), E);

summary(Summary, E) when is_list(Summary) ->
    E#{ summary := Summary }.

%%%
%%% eunit
%%%
-ifdef(TEST).
summary_test() ->
    R = new("http://example.org:8081/myresource", "http://example.org/occi#type"),
    R0 = summary(<<"my summary">>, R),
    ?assertMatch("my summary", summary(R0)).

-endif.
