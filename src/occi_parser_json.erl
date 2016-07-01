%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_json).

-include("occi_log.hrl").
-include("occi_entity.hrl").

-export([parse/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec parse(binary()) -> occi_rendering:ast().
parse(<<>>) ->
    throw({parse_error, empty});
    
parse(Bin) ->
    maps:fold(fun validate/3, #{}, jsx:decode(Bin, [return_maps])).

%%%
%%% Parsers
%%%
validate(<<"kind">>, V, Acc) when is_binary(V) ->
    %% resource: String
    %% link: String
    Acc#{ kind => type_id(V) };

validate(<<"mixins">>, V, Acc) when is_list(V) ->
    %% resource: Array
    %% link: Array
    %% collection: Array
    Acc#{ mixins => [ val_mixin(Bin) || Bin <- V ] };

validate(<<"kinds">>, V, Acc) when is_list(V) ->
    %% collection: Array
    Acc#{ kinds => [ val_kind(Bin) || Bin <- V ] };

validate(<<"attributes">>, V, Acc) when is_map(V) ->
    %% resource: Object
    %% action invocation: Object
    %% link: Object
    %% kind: Object
    %% action: Object
    Acc#{ attributes => maps:merge(maps:get(attributes, Acc, #{}), V) };

validate(<<"actions">>, V, Acc) when is_list(V) ->
    %% resource: Array
    %% link: Array
    %% kind: Array
    %% collections: Array
    Acc#{ actions => [ val_action(Bin) || Bin <- V ] };

validate(<<"id">>, V, Acc) when is_binary(V) ->
    %% resource: String
    %% link: String
    Acc#{ attributes => maps:put(<<"occi.core.id">>, V, maps:get(attributes, Acc, #{})) };

validate(<<"links">>, V, Acc) when is_list(V) ->
    %% resource: Array
    %% collection: Array
    Acc#{ links => [ val_link(Bin) || Bin <- V ] };

validate(<<"summary">>, V, Acc) when is_binary(V) ->
    %% resource: String
    Acc#{ attributes => maps:put(<<"occi.core.summary">>, V, maps:get(attributes, Acc, #{})) };

validate(<<"title">>, V, Acc) when is_binary(V) ->
    %% resource: String
    %% link: String
    %% kind: String
    %% action: String
    Acc#{ title => V,
	  attributes => maps:put(<<"occi.core.title">>, V, maps:get(attributes, Acc, #{})) };

validate(<<"action">>, V, Acc) when is_binary(V) ->
    %% action invocation: String
    Acc#{ action => type_id(V) };

validate(<<"source">>, V, Acc) when is_map(V) ->
    %% link: Object
    Source = maps:fold(fun link_end/3, #{}, V),
    Acc#{ source => Source };

validate(<<"target">>, V, Acc) when is_map(V) ->
    %% link: Object
    Target = maps:fold(fun link_end/3, #{}, V),
    Acc#{ target => Target };

validate(<<"term">>, V, Acc) when is_binary(V) ->
    %% kind: String
    %% mixin: String
    %% action: String
    Acc#{ term => V };

validate(<<"scheme">>, V, Acc) when is_binary(V) ->
    %% kind: String
    %% mixin: String
    %% action: String
    Acc#{ scheme => V };

validate(<<"parent">>, V, Acc) when is_binary(V) ->
    %% kind: String
    Acc#{ parent => type_id(V) };

validate(<<"location">>, V, Acc) when is_binary(V) ->
    %% kind: String
    %% mixin: String
    Acc#{ location => V };

validate(<<"depends">>, V, Acc) when is_list(V) ->
    %% mixin: Array
    Acc#{ depends => [ type_id(Bin) || Bin <- V ] };

validate(<<"applies">>, V, Acc) when is_list(V) ->
    %% mixin: Array
    Acc#{ applies => [ type_id(Bin) || Bin <- V ] };

validate(<<"resources">>, V, Acc) when is_list(V) ->
    %% collection: Array
    Acc#{ resources => [ val_resource(Bin) || Bin <- V ] }.



link_end(<<"location">>, V, Acc) when is_binary(V) ->
    Acc#{ location => V };

link_end(<<"kind">>, V, Acc) when is_binary(V) ->
    Acc#{ kind => type_id(V) }.


val_kind(V) when is_map(V) ->
    Acc0 = #{},
    maps:fold(fun val_kind/3, Acc0, V).
    

val_kind(<<"term">>, V, Acc) when is_binary(V) ->
    Acc#{ term => V };

val_kind(<<"scheme">>, V, Acc) when is_binary(V) ->
    Acc#{ scheme => V };

val_kind(<<"title">>, V, Acc) when is_binary(V) ->
    Acc#{ title => V };

val_kind(<<"attributes">>, V, Acc) when is_map(V) ->
    Acc#{ attributes => maps:fold(fun val_attributes/3, #{}, V) };

val_kind(<<"actions">>, V, Acc) when is_list(V) ->
    Acc#{ actions => [ type_id(Bin) || Bin <- V ]};

val_kind(<<"parent">>, V, Acc) when is_list(V) ->
    Acc#{ parent => type_id(V) };
    
val_kind(<<"location">>, V, Acc) when is_binary(V) ->
    Acc#{ location => V }.


val_mixin(V) when is_binary(V) ->
    type_id(V);

val_mixin(V) when is_map(V) ->
    Acc0 = #{},
    maps:fold(fun val_mixin/3, Acc0, V).


val_mixin(<<"term">>, V, Acc) when is_binary(V) ->
    Acc#{ term => V };

val_mixin(<<"scheme">>, V, Acc) when is_binary(V) ->
    Acc#{ scheme => V };

val_mixin(<<"title">>, V, Acc) when is_binary(V) ->
    Acc#{ title => V };

val_mixin(<<"attributes">>, V, Acc) when is_map(V) ->
    Acc#{ attributes => maps:fold(fun val_attributes/3, #{}, V) };

val_mixin(<<"actions">>, V, Acc) when is_list(V) ->
    Acc#{ actions => [ type_id(Bin) || Bin <- V ]};

val_mixin(<<"depends">>, V, Acc) when is_list(V) ->
    Acc#{ depends => [ type_id(Bin) || Bin <- V ]};
    
val_mixin(<<"applies">>, V, Acc) when is_list(V) ->
    Acc#{ applies => [ type_id(Bin) || Bin <- V ]};

val_mixin(<<"location">>, V, Acc) when is_binary(V) ->
    Acc#{ location => V }.


val_action(V) when is_binary(V) ->
    type_id(V);

val_action(V) when is_map(V) ->
    Acc0 = #{},
    maps:fold(fun val_action/3, Acc0, V).


val_action(<<"term">>, V, Acc) when is_binary(V) ->
    Acc#{ term => V };

val_action(<<"scheme">>, V, Acc) when is_binary(V) ->
    Acc#{ scheme => V };

val_action(<<"title">>, V, Acc) when is_binary(V) ->
    Acc#{ title => V };

val_action(<<"attributes">>, V, Acc) when is_map(V) ->
    Acc#{ attributes => maps:fold(fun val_attributes/3, #{}, V) };

val_action(<<"location">>, V, Acc) when is_binary(V) ->
    Acc#{ location => V }.


val_link(V) when is_map(V) ->
    maps:fold(fun val_link/3, #{}, V).


val_link(<<"kind">>, V, Acc) when is_binary(V) ->
    Acc#{ kind => type_id(V) };

val_link(<<"mixins">>, V, Acc) when is_list(V) ->
    Acc#{ mixins => [ type_id(Bin) || Bin <- V ] };

val_link(<<"attributes">>, V, Acc) when is_map(V) ->
    Acc#{ attributes => maps:merge(maps:get(attributes, Acc, #{}), V) };

val_link(<<"actions">>, V, Acc) when is_list(V) ->
    Acc#{ actions => [ type_id(Bin) || Bin <- V ] };

val_link(<<"id">>, V, Acc) when is_binary(V) ->
    Acc#{ attributes => maps:put(<<"occi.core.id">>, V, maps:get(attributes, Acc, #{})) };

val_link(<<"location">>, V, Acc) when is_binary(V) ->
    Acc#{ location => V };

val_link(<<"title">>, V, Acc) when is_binary(V) ->
    Acc#{ attributes => maps:put(<<"occi.core.title">>, V, maps:get(attributes, Acc, #{})) };

val_link(<<"source">>, V, Acc) when is_map(V) ->
    Source = maps:fold(fun link_end/3, #{}, V),
    Acc#{ source => Source };

val_link(<<"target">>, V, Acc) when is_map(V) ->
    Target = maps:fold(fun link_end/3, #{}, V),
    Acc#{ target => Target }.


val_resource(V) when is_map(V) ->
    maps:fold(fun val_resource/3, #{}, V).


val_resource(<<"kind">>, V, Acc) when is_binary(V) ->
    Acc#{ kind => type_id(V) };

val_resource(<<"mixins">>, V, Acc) when is_binary(V) ->
    Acc#{ mixins => [ type_id(Bin) || Bin <- V ] };

val_resource(<<"attributes">>, V, Acc) when is_map(V) ->
    Acc#{ attributes => maps:merge(maps:get(attributes, Acc, #{}), V) };

val_resource(<<"actions">>, V, Acc) when is_list(V) ->
    Acc#{ actions => [ type_id(Bin) || Bin <- V ] };

val_resource(<<"id">>, V, Acc) when is_binary(V) ->
    Acc#{ attributes => maps:put(<<"occi.core.id">>, V, maps:get(attributes, Acc, #{})) };

val_resource(<<"links">>, V, Acc) when is_list(V) ->
    Acc#{ links => [ val_link(Bin) || Bin <- V ] };

val_resource(<<"title">>, V, Acc) when is_binary(V) ->
    Acc#{ title => maps:put(<<"occi.core.title">>, V, maps:get(attributes, Acc, #{})) };

val_resource(<<"summary">>, V, Acc) when is_binary(V) ->
    Acc#{ summary => maps:put(<<"occi.core.summary">>, V, maps:get(attributes, Acc, #{})) };

val_resource(<<"location">>, V, Acc) when is_binary(V) ->
    Acc#{ location => V }.


val_attributes(<<"mutable">>, V, Acc) when is_boolean(V) ->
    Acc#{ mutable => V };

val_attributes(<<"required">>, V, Acc) when is_boolean(V) ->
    Acc#{ required => V };

val_attributes(<<"type">>, V, Acc) when is_binary(V) ->
    Acc#{ type => V };

val_attributes(<<"pattern">>, V, Acc) when is_map(V) ->
    Acc#{ pattern => V };

val_attributes(<<"default">>, V, Acc) when is_binary(V);
					   is_integer(V);
					   is_float(V);
					   is_boolean(V);
					   is_list(V);
					   is_map(V) ->
    Acc#{ default => V };

val_attributes(<<"description">>, V, Acc) when is_binary(V) ->
    Acc#{ description => V }.


type_id(Bin) ->
    occi_category:parse_id(Bin).

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
