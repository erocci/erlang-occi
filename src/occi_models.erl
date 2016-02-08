%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_models).

-export([init/0]).
-on_load(init/0).

-export([category/1,
	 attribute/2,
	 attributes/1]).

-record category, {id      :: occi_category:id(),
		   value   :: occi_category:t()}.

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
    ok = core_categories(),
    ok.

-spec category(occi_category:id()) -> occi_category:t() | undefined.
category(Id) ->
    case mnesia:dirty_read(category, Id) of
	[] -> undefined;
	[#category{value=C}] -> C
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
    Op = fun() ->
		 E = occi_entity:category(),
		 ok = mnesia:write(category, #category{id=occi_category:id(E), value=E}, write),
		 R = occi_resource:category(),
		 ok = mnesia:write(category, #category{id=occi_category:id(R), value=R}, write),
		 L = occi_link:category(),
		 ok = mnesia:write(category, #category{id=occi_category:id(L), value=L}, write)
	 end,
    {atomic, ok} = mnesia:transaction(Op),
    ok.		 

%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
