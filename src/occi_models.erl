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
-include_lib("annotations/include/annotations.hrl").

-export([start_link/0]).

-export([import/1,
	 categories/0,
	 category/1,
	 add_category/1,
	 attribute/2,
	 attributes/1]).

%% internal
-define(core_scheme, "http://schemas.ogf.org/occi/core#").

-record category, {id        :: occi_category:id(),
		   value     :: occi_category:t()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec start_link() -> {ok, pid()}.
start_link() ->
    Pid = spawn_link(fun init/0),
    {ok, Pid}.


%% @doc Import an extension into the model
%%
%% @end
-spec import(occi_extension:t()) -> ok.
import(E) ->
    ok = load_imports(occi_extension:imports(E)),
    ok = load_categories(occi_extension:scheme(E), occi_extension:kinds(E)),
    ok = load_categories(occi_extension:scheme(E), occi_extension:mixins(E)).


%% @doc Return the list of categories
%% Categories references (parent, depends, etc) are not resolved
%%
%% @end
-spec categories() -> [occi_category:t()].
categories() ->
    case mnesia:transaction(fun categories_t/0) of
	{aborted, Err} -> throw(Err);
	{atomic, Categories} -> Categories
    end.


%% @doc Return a category
%% Category references (parent, depend, etc) is resolved:
%% attributes and actions from references are merged into the resulting category
%%
%% @end
-spec category(occi_category:id()) -> occi_category:t() | undefined.
category(Id) ->
    case mnesia:transaction(fun () -> category_t(Id) end) of
	{aborted, Err} -> throw(Err);
	{atomic, C} -> C
    end.


-spec add_category(occi_category:t()) -> ok.
add_category(Cat) ->
    C = #category{id=occi_category:id(Cat), value=Cat},
    case mnesia:transaction(fun() -> mnesia:write(C) end) of
	{atomic, ok} ->
	    ok;
	{aborted, Err} ->
	    throw(Err)
    end.


%% @throws {unknown_category, term()}
%%-logging(debug).
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
-spec attributes(occi_category:id()) -> [occi_attribute:t()].
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
-spec init() -> ok.
init() ->
    case mnesia:create_table(category, [{ram_copies, nodes()}, {attributes, record_info(fields, category)}]) of
	{atomic, ok} -> ok;
	{aborted, {already_exists, category}} -> ok;
	{aborted, _} = Err -> throw(Err)
    end,
    ok = load_imports([?core_scheme]),
    loop().


loop() ->
    receive
	_ ->
	    loop()
    end.


%% Check extension exists in the database, throw error if not
load_imports([]) ->
    ok;

load_imports([ Scheme | Imports ]) ->
    ?debug("Import extension: ~s", [Scheme]),
    case dl_schema(Scheme) of
	{ok, Path} ->
	    case file:read_file(Path) of
		{ok, Bin} ->
		    import(occi_extension:load(occi_utils:mimetype(Path), Bin));
		{error, Err} ->
		    throw({import, Err})
	    end,
	    load_imports(Imports);
	{error, Err} ->
	    throw({import, Err})
    end.


load_categories(_Scheme, []) ->
    ok;

load_categories(Scheme, [ Cat | Categories ]) ->
    ?debug("Add category: ~p", [occi_category:id(Cat)]),
    ok = add_category(Cat),
    load_categories(Scheme, Categories).


dl_schema(Scheme) ->
    Urls = [{baseurl() ++ "/" ++ http_uri:encode(http_uri:encode(Scheme)) ++ ".xml", http_uri:encode(Scheme) ++ ".xml"}],
    occi_dl:resource(Scheme, Urls).    


baseurl() ->
    case application:get_env(occi, schemas_baseurl, undefined) of
	undefined ->
	    throw({undefined_env, schemas_baseurl});
	{priv_dir, Dir} ->
	    filename:join([occi_utils:priv_dir(), Dir]);
	Dir when is_list(Dir) ->
	    Dir
    end.

%%%
%%% Requests functions
%%%
categories_t() ->
    mnesia:foldl(fun (#category{value=Cat}, Acc) ->
			 [ Cat | Acc ]
		 end, [], category).


-spec category_t(occi_category:id()) -> occi_category:t() | undefined.
category_t(Id) ->
    case mnesia:read(category, Id, read) of
	[] -> 
	    undefined;
	[#category{value=C}] -> 
	    resolve_t(occi_category:class(C), C)
    end.


-spec resolve_t(occi_category:class(), occi_category:t()) -> occi_category:t().
resolve_t(kind, C) ->
    case occi_kind:parent(C) of
	undefined ->
	    C;
	ParentId ->
	    case category_t(ParentId) of
		undefined ->
		    mnesia:abort({invalid_parent, ParentId});
		Parent ->
		    occi_kind:parents([ParentId | occi_kind:parents(Parent) ], C)
	    end
    end;

resolve_t(mixin, C) ->
    C.

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
