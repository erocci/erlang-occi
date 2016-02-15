%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_models).

-include("occi_log.hrl").

-export([init/0]).
-on_load(init/0).

-export([category/1,
	 add_category/2,
	 attribute/2,
	 attributes/1]).

%% Defines mimetype as in cowlib

%% internal
-define(core_scheme, "http://schemas.ogf.org/occi/core#").

-record category, {id        :: occi_category:id(),
		   extension :: occi_extension:id(),
		   value     :: occi_category:t()}.

-type extension_status() :: loaded | pending | {error, term()}.
-record extension, {scheme    :: occi_extension:id(), status    :: extension_status()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Called on module load. Allows unit testing.
-spec init() -> ok.
init() ->
    {ok, _} = application:ensure_all_started(mnesia),
    case mnesia:create_table(category, [{ram_copies, nodes()}, {attributes, record_info(fields, category)}]) of
	{atomic, ok} -> ok;
	{aborted, {already_exists, category}} -> ok;
	{aborted, _} = Err -> throw(Err)
    end,
    case mnesia:create_table(extension, [{ram_copies, nodes()}, {attributes, record_info(fields, extension)}]) of
	{atomic, ok} -> ok;
	{aborted, {already_exists, category}} -> ok;
	{aborted, _} = Err2 -> throw(Err2)
    end,
    ok = core_categories(),
    ok.


-spec category(occi_category:id()) -> occi_category:t() | undefined.
category(Id) ->
    case mnesia:dirty_read(category, Id) of
	[] -> undefined;
	[#category{value=C}] -> C
    end.


-spec add_category(occi_extension:id(), occi_category:t()) -> ok.
add_category(Scheme, Cat) ->
    C = #category{id=occi_category:id(Cat), extension=Scheme, value=Cat},
    case mnesia:transaction(fun() -> mnesia:write(C) end) of
	{atomic, ok} ->
	    ok;
	{aborted, Err} ->
	    throw(Err)
    end.


%% @throws {unknown_category, term()}
-spec attribute(occi_attribute:key(), [occi_category:id()]) -> occi_attribute:t() | undefined.
attribute(_Key, []) ->
    undefined;

attribute(Key, [ CatId | Others ]) ->
    case category(CatId) of
	undefined ->
	    throw({unknown_category, CatId});
	Cat ->
	    case occi_category:attribute(Key, Cat) of
		undefined ->
		    attribute(Key, Others);
		Attr ->
		    Attr
	    end
    end.

%% @throws {unknown_category, occi_category:id()}
-spec attributes(occi_category:id()) -> map().
attributes(CatId) ->
    case category(CatId) of
	undefined ->
	    throw({unknown_category, CatId});
	Cat ->
	    occi_category:attributes(Cat)
    end.

%%
%% Priv
%%
core_categories() ->
    ok = add_category(?core_scheme, occi_category:entity()),
    ok = add_category(?core_scheme, occi_category:resource()),
    ok = add_category(?core_scheme, occi_category:link_()),
    ok.

%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
