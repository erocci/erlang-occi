%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_category).

-include("occi_uri.hrl").
-include("occi_category.hrl").

-export([new/2, 
	 new/3,
	 id/1,
	 class/1,
	 title/1,
	 title/2,
	 attribute/2,
	 add_attribute/2,
	 attributes/1,
	 add_action/2,
	 actions/1,
	 location/1,
	 location/2]).

-export([render/3]).

-export([parse_id/1]).

-type class() :: kind | mixin | action.
-type id() :: {Scheme :: binary(), Term :: binary()}.


-record(category, {id :: id(), m :: #{}}).
-type t() :: #category{}.

-export_type([id/0, class/0, t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {invalid_cid, term()}
-spec new(Id :: string() | id(), Cls :: class()) -> t().
new(CatId, Cls) when is_binary(CatId) ->
    new(parse_id(CatId), Cls);

new({Scheme, Term}, Cls) when Cls =:= kind; Cls =:= mixin; Cls =:= action ->
    {Cls, {Scheme, Term}, #{
	    title => "",
	    attributes => #{}
	   }};

new({<<"http://schemas.ogf.org/occi/core#">>, <<"entity">>}=Id, kind) ->
    {kind, Id, #{ 
	     title => "Entity",
	     attributes => #{} 
	    }}.


%% throws {invalid_cid, {term(), term()}}
-spec new(Scheme :: binary(), Term :: binary(), Cls :: class()) -> t().
new(Scheme, Term, Cls) when is_binary(Scheme), is_binary(Term) ->
    new({Scheme, Term}, Cls);

new(Scheme, Term, _Cls) ->
    throw({invalid_cid, {Scheme ,Term}}).


-spec class(t()) -> class().
class(C) ->
    element(1, C).


-spec id(t()) -> occi_category:id().
id(C) ->
    element(2, C).


-spec title(t()) -> binary().
title(C) ->
    ?g(title, C).


-spec title(binary(), t()) -> t().
title(Title, C) when is_binary(Title) ->
    ?s(title, Title, C).


-spec attribute(occi_attribute:key(), t()) -> occi_attribute:t().
attribute(Key, C) when is_binary(Key) ->
    Attrs = ?g(attributes, C),
    maps:get(Key, Attrs, undefined).


-spec add_attribute(occi_attribute:t(), t()) -> t().
add_attribute(Attr, C) ->
    Attrs = ?g(attributes, C),
    ?s(attributes, Attrs#{ occi_attribute:name(Attr) => Attr }, C).


-spec attributes(t()) -> [occi_attribute:t()].
attributes(C) ->
    Attrs = ?g(attributes, C),
    maps:values(Attrs).


-spec add_action(occi_action:t(), t()) -> t().
add_action(Action, {Cls, _,  M}=C) when Cls =:= kind; Cls =:= mixin ->
    Actions = maps:get(actions, M),
    ?s(actions, Actions#{ occi_action:id(Action) => Action}, C).


-spec actions(t()) -> [occi_action:t()].
actions(C) ->
    Actions = ?g(actions, C),
    maps:values(Actions).


-spec location(t()) -> uri:t().
location(C) ->
    ?g(location, C).


-spec location(uri:t(), t()) -> t().
location(Location, C) when ?is_uri(Location) ->
    ?s(location, Location, C).


%% @throws {invalid_cid, term()}
-spec parse_id(binary()) -> id().
parse_id(Id) when is_binary(Id) ->
    try uri:from_string(Id) of
	Uri ->
	    Scheme = << (uri:to_string(uri:frag(Uri, <<>>)))/binary, "#" >>,
	    Term = case uri:frag(Uri) of
		       <<>> -> throw({invalid_cid, Id});
		       T -> T
		   end,
	    {Scheme, Term}
    catch
	error:{badmatch, _} ->
	    throw({invalid_cid, Id})
    end;

parse_id(Id) ->
    throw({invalid_cid, Id}).


%% @doc Render category into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), render_ctx()) -> iolist().
render(Mimetype, E, Ctx) ->
    occi_rendering:render(Mimetype, E, Ctx).


%%%
%%% eunit
%%%
-ifdef(TEST).
parse_id_test_() ->
    [
     ?_assertThrow({invalid_cid, <<>>}, parse_id(<<>>)),
     ?_assertThrow({invalid_cid, <<"bad_scheme">>}, parse_id(<<"bad_scheme">>)),
     ?_assertThrow({invalid_cid, <<"http://example.org/occi#">>}, parse_id(<<"http://example.org/occi#">>)),
     ?_assertMatch({<<"http://example.org/occi#">>, <<"t">>}, 
		   parse_id(<<"http://example.org/occi#t">>)),
     ?_assertMatch({<<"http://example.org/occi#">>, <<"t">>}, 
		   parse_id(<<"http://example.org/occi#t">>))
    ].

-endif.
