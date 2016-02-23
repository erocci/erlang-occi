%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc The module is used to access the full model of an OCCI endpoint, 
%%% ie, more or less extensions with resolved imports and location associated
%%% to each kind or mixin.
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_models).

-include("occi_log.hrl").

-export([init/0]).
-on_load(init/0).

-export([import/1,
	 import/2,
	 category/1,
	 find/1,
	 add_category/3,
	 attribute/2,
	 attributes/1]).

%% internal
-define(core_scheme, "http://schemas.ogf.org/occi/core#").

-record category, {id        :: occi_category:id(),
		   extension :: occi_extension:id(),
		   location  :: string(),
		   value     :: occi_category:t()}.

-type hash_t() :: {default, Prefix :: string()}
		| mfa().

-define(default_hash, {default, "categories"}).
-export_type([hash_t/0]).

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


%% @doc Equivalent import(E, {default, "categories"})
%% @end
-spec import(occi_extension:t()) -> ok.
import(E) ->
    import(E, ?default_hash).


%% @doc Import an extension into the model
%% Hash is the method used to attribute an url to the category collection.
%% <ul>
%%   <li><em>'default':</em> location is of the form /categories/&lt;name&gt; where 
%%   &lt;name&gt; is the term of the category, eventually suffix'ed with a number</li>
%%   <li><em>mfa():</em>fun((Scheme, Term, State) -> {string(), State})
%% </ul>
%%
%% @end
-spec import(occi_extension:t(), hash_t()) -> ok.
import(E, Hash) ->
    ok = load_imports(occi_extension:imports(E), Hash),
    ok = load_categories(occi_extension:scheme(E), occi_extension:kinds(E), Hash),
    ok = load_categories(occi_extension:scheme(E), occi_extension:mixins(E), Hash).


-spec category(occi_category:id()) -> occi_category:t() | undefined.
category(Id) ->
    case mnesia:dirty_read(category, Id) of
	[] -> undefined;
	[#category{value=C}] -> C
    end.


%% @doc Find a category by location
-spec find(string()) -> occi_category:t() | undefined.
find(Location) ->
    case mnesia:dirty_match_object(#category{location=Location, _='_'}) of
	[] -> undefined;
	[#category{value=C}] -> C
    end.


-spec add_category(occi_extension:id(), occi_category:t(), hash_t()) -> ok.
add_category(Scheme, Cat, Location) ->
    Cat0 = occi_category:location(Location, Cat),
    C = #category{id=occi_category:id(Cat0), extension=Scheme, location=Location, value=Cat0},
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
add_category(Scheme, Cat) ->
    Location = hash_location(occi_category:id(Cat), ?default_hash),
    add_category(Scheme, Cat, Location).


core_categories() ->
    ok = add_category(?core_scheme, occi_category:entity()),
    ok = add_category(?core_scheme, occi_category:resource()),
    ok = add_category(?core_scheme, occi_category:link_()),
    ok.

%% Check extension exists in the database, throw error if not
load_imports([], _) ->
    ok;

load_imports([ Scheme | Imports ], Hash) ->
    ?debug("Import extension: ~s", [Scheme]),
    Urls = [{baseurl() ++ "/" ++ http_uri:encode(http_uri:encode(Scheme)) ++ ".xml", http_uri:encode(Scheme) ++ ".xml"}],
    case occi_dl:resource(Scheme, Urls) of
	{ok, Path} ->
	    import(occi_extension:load_path(Path)),
	    load_imports(Imports, Hash);
	{error, Err} ->
	    throw({import, Err})
    end.


load_categories(_Scheme, [], _Hash) ->
    ok;

load_categories(Scheme, [ Cat | Categories ], Hash) ->
    ?debug("Add category: ~p", [occi_category:id(Cat)]),
    Location = hash_location(occi_category:id(Cat), Hash),
    ok = add_category(Scheme, Cat, Location),
    load_categories(Scheme, Categories, Hash).


baseurl() ->
    case application:get_env(occi, schemas_baseurl, undefined) of
	undefined ->
	    throw({undefined_env, schemas_baseurl});
	{priv_dir, Dir} ->
	    filename:join([occi_utils:priv_dir(), Dir]);
	Dir when is_list(Dir) ->
	    Dir
    end.


hash_location({_Scheme, Term}, {default, Prefix}) when is_list(Prefix) ->
    hash(Term, Prefix, 0);

hash_location(Id, {M, F, [A]}) ->
    erlang:apply(M, F, [Id, A]).


hash(Term, Prefix, 0) ->
    Location = filename:join([Prefix, Term]),
    case find(Location) of
	undefined -> Location;
	_ -> hash(Term, Prefix, 1)
    end;

hash(Term, Prefix, I) ->
    Location = filename:join([Prefix, lists:flatten(io_lib:format("~s.~b", [Term, I]))]),
    case find(Location) of
	undefined -> Location;
	_ -> hash(Term, Prefix, I+1)
    end.


%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
