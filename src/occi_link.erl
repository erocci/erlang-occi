%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_link).

-include_lib("mixer/include/mixer.hrl").

-mixin([{occi_entity, except, [new/1, new/2, category/0]}]).

-export([new/3, 
	 new/4,
	 source/1,
	 target/1]).

-type t() :: occi_entity:t().
-export_type([t/0]).

-define(category_id, {"http://schemas.ogf.org/occi/core#", "link"}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @throws {invalid_uri, iolist()}
-spec new(uri:t() | string() | binary(),
	  uri:t() | string() | binary(),
	  uri:t() | string() | binary()) -> t().
new(Id, Src, Target) ->
    new(Id, ?category_id, Src, Target).


%% @throws {invalid_uri, iolist()}
%% @throws {unknown_category, term()}
-spec new(uri:t() | string() | binary(), 
	  occi_category:id() | string() | binary(),
	  uri:t() | string() | binary(),
	  uri:t() | string() | binary()) -> t().
new(Id, KindId, Src, Target) ->
    Entity = occi_entity:new(Id, KindId),
    SrcUri = try uri:from_string(Src) of
		 V0 -> V0
	     catch
		 error:{badmatch, _} ->
		     throw({invalid_uri, Src})
	     end,
    TargetUri = try uri:from_string(Target) of
		    V1 -> V1
		catch
		    error:{badmatch, _} ->
			throw({invalid_uri, Target})
		end,
    Entity#{ source => SrcUri, target => TargetUri }.


-spec source(t()) -> uri:t().
source(E) ->
    maps:get(source, E).


-spec target(t()) -> uri:t().
target(E) ->
    maps:get(target, E).

%%%
%%% eunit
%%%
-ifdef(TEST).
core() ->
    R = new("http://example.org/mylink0", "http://example.org/resource0", "http://example.org/resource1"),
    ?assertEqual(?category_id, kind(R)).

new_throw_() ->
    [
     ?_assertThrow({invalid_uri, ""}, new("http://example.org/mylink0", "", ""))
    ].
-endif.
