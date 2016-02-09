%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_category).

-export([new/2, 
	 new/3,
	 id/1,
	 class/1,
	 title/1,
	 title/2,
	 attribute/2,
	 add_attribute/2,
	 attributes/1,
	 add_action/2]).

-export([entity/0,
	 resource/0,
	 link_/0]).

-export([parse_id/1]).

-type class() :: kind | mixin | action.
-type id() :: {Scheme :: string(), Term :: string()}.

-type t() :: #{}.

-export_type([id/0, class/0, t/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {invalid_cid, term()}
-spec new(Id :: string() | id(), Cls :: class()) -> t().
new(CatId, Cls) when is_list(CatId); is_binary(CatId) ->
    new(parse_id(CatId), Cls);

new({Scheme, Term}, Cls) when Cls =:= kind; Cls =:= mixin; Cls =:= action ->
    #{
       id => {Scheme, Term},
       title => "",
       class => Cls,
       attributes => #{}
     };

new({"http://schemas.ogf.org/occi/core#", "entity"}=Id, kind) ->
    #{ 
     id => Id,
     title => "Entity",
     class => kind,
     attributes => "" 
    }.


%% throws {invalid_cid, {term(), term()}}
-spec new(Scheme :: string(), Term :: string(), Cls :: class()) -> t().
new(Scheme, Term, Cls) when is_list(Scheme), is_list(Term) ->
    new({Scheme, Term}, Cls);

new(Scheme, Term, _Cls) ->
    throw({invalid_cid, {Scheme ,Term}}).


-spec class(t()) -> class().
class(C) ->
    maps:get(class, C).


-spec id(t()) -> occi_category:id().
id(C) ->
    maps:get(id, C).


-spec title(t()) -> string().
title(C) ->
    maps:get(title, C).


-spec title(string(), t()) -> t().
title(Title, C) ->
    C#{ title := Title }.


-spec attribute(occi_attribute:key(), t()) -> occi_attribute:t().
attribute(Key, C) ->
    Attrs = maps:get(attributes, C),
    map:get(Key, Attrs, undefined).


-spec add_attribute(occi_attribute:t(), t()) -> t().
add_attribute(Attr, C) ->
    Attrs = maps:get(attributes, C),
    C#{ attributes := Attrs#{ occi_attribute:name(Attr) => Attr } }.


-spec attributes(occi_category:t()) -> map().
attributes(C) ->
    maps:get(attributes, C).


-spec add_action(occi_action:t(), t()) -> t().
add_action(Action, #{ class := Cls}=C) when Cls =:= kind; Cls =:= mixin ->
    Actions = maps:get(actions, C),
    C#{ actions := Actions#{ occi_action:id(Action) => Action} }.


%% @throws {invalid_cid, term()}
-spec parse_id(string() | binary()) -> id().
parse_id(Id) when is_list(Id); is_binary(Id) ->
    try uri:from_string(Id) of
	Uri ->
	    Scheme = binary_to_list(uri:to_string(uri:frag(Uri, <<>>))) ++ "#",
	    Term = case uri:frag(Uri) of
		       <<>> -> throw({invalid_cid, Id});
		       T -> binary_to_list(T)
		   end,
	    Term = binary_to_list(uri:frag(Uri)),
	    {Scheme, Term}
    catch
	error:{badmatch, _} ->
	    throw({invalid_cid, Id})
    end;

parse_id(Id) ->
    throw({invalid_cid, Id}).

%%%
%%% constructors
%%%
entity() ->
    #{ id => {"http://schemas.ogf.org/occi/core#", "entity"},
       title => "Entity",
       class => kind,
       attributes => #{} }.


resource() ->
    #{ id => {"http://schemas.ogf.org/occi/core#", "resource"},
       title => "Resource",
       class => kind,
       attributes => #{} }.


link_() ->
    #{ id => {"http://schemas.ogf.org/occi/core#", "link"},
       title => "Link",
       class => kind,
       attributes => #{} }.

%%%
%%% eunit
%%%
-ifdef(TEST).
parse_id_test_() ->
    [
     ?_assertThrow({invalid_cid, ""}, parse_id("")),
     ?_assertThrow({invalid_cid, "bad_scheme"}, parse_id("bad_scheme")),
     ?_assertThrow({invalid_cid, "http://example.org/occi#"}, parse_id("http://example.org/occi#")),
     ?_assertMatch({"http://example.org/occi#", "t"}, 
		   parse_id("http://example.org/occi#t")),
     ?_assertMatch({"http://example.org/occi#", "t"}, 
		   parse_id(<<"http://example.org/occi#t">>))
    ].

-endif.
