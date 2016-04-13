%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc The module is used to access the full model of an OCCI endpoint, 
%%% ie, more or less extensions with resolved imports and location associated
%%% to each kind or mixin.
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_models).

-include("occi.hrl").
-include("occi_type.hrl").
-include("occi_log.hrl").
-include_lib("annotations/include/annotations.hrl").

-export([start_link/0]).

-export([import/1,
	 categories/0,
	 category/1,
	 kind/2,
	 action/1,
	 location/1,
	 add_category/1,
	 attribute/2,
	 attributes/1]).

%% internal
-define(core_scheme, <<"http://schemas.ogf.org/occi/core#">>).

-record category, {id        :: occi_category:id(),
		   location  :: binary(),
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


%% @doc Return a kind, checking it has specified parent 
%% @throws {unknown_category, occi_category:id()} | {invalid_kind, occi_category:id()}
%% @end
-spec kind(link | resource, occi_category:id()) -> occi_category:t().
kind(Parent, KindId) when is_binary(KindId) ->
    kind(Parent, occi_category:parse_id(KindId));

kind(Parent, KindId) when ?is_category_id(KindId) ->
    case category(KindId) of
	undefined ->
	    throw({unknown_category, KindId});
	Category ->
	    case occi_kind:has_parent(Parent, Category) of
		true -> Category;
		false -> throw({invalid_kind, KindId})
	    end
    end.


%% @doc Return an action definition
%% @end
-spec action(occi_category:id()) -> occi_action:t().
action(ActionId) ->
    case category(ActionId) of
	undefined ->
	    throw({unknown_action, ActionId});
	Action when ?is_action(Action) ->
	    Action;
	_ ->
	    throw({not_an_action, ActionId})
    end.


%% @doc If any, return a category given a location.
%% Otherwise, returns 'undefined
%% @end
-spec location(binary()) -> occi_category:t() | undefined.
location(Path) ->
    Fun = fun () ->
		  mnesia:match_object(#category{_='_', location=Path})
	  end,		  
    case mnesia:transaction(Fun) of
	{aborted, Err} -> throw(Err);
	{atomic, []} -> undefined;
	{atomic, [#category{value=Category}]} -> Category
    end.		  


%% @doc Return a category
%% Category references (parent, depend, etc) is resolved:
%% attributes and actions from references are merged into the resulting category
%%
%% @end
-spec category(binary() | occi_category:id()) -> occi_category:t() | undefined.
category(Id) when is_binary(Id) ->
    category(occi_category:parse_id(Id));

category(Id) when ?is_category_id(Id) ->
    case mnesia:transaction(fun () -> category_t(Id) end) of
	{aborted, Err} -> throw(Err);
	{atomic, C} -> C
    end.


-spec add_category(occi_category:t()) -> ok.
add_category(Cat) ->
    case mnesia:transaction(fun () -> add_category_t(Cat) end) of
	{atomic, ok} -> ok;
	{aborted, Err} -> throw(Err)
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
-define(model_ctx, #parse_ctx{ valid=model, url=undefined }).

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
    lists:foreach(fun (Action) ->
			  ok = add_category(Action)
		  end, occi_category:actions(Cat)),
    load_categories(Scheme, Categories).


dl_schema(Scheme) ->
    S = http_uri:encode(binary_to_list(Scheme)),
    Urls = [{baseurl() ++ "/" ++ http_uri:encode(S) ++ ".xml", S ++ ".xml"}],
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


hash_location(Term) ->
    Prefix = application:get_env(occi, collections_prefix, <<"/">>),
    Loc = filename:join([Prefix, Term]),
    hash_location2(exists_location(Loc), Loc, 0).


hash_location2(false, Loc, _I) ->
    Loc;

hash_location2(true, Loc, I) ->
    Loc2 = << Loc/binary, (integer_to_binary(I))/binary >>,
    hash_location2(exists_location(Loc2), Loc2, I+1).


exists_location(Loc) ->
    case mnesia:dirty_match_object(#category{_='_', location=Loc}) of
	[] ->  false;
	_ -> true
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
    C;

resolve_t(action, C) ->
    C.


add_category_t(Cat) ->
    case mnesia:read(category, occi_category:id(Cat)) of
	[] -> 
	    mnesia:write(category_record_t(Cat));
	_ ->
	    %% Ignore duplicate category
	    ok
    end.


category_record_t(Cat) when ?is_action(Cat) ->
    #category{id=occi_category:id(Cat), location=undefined, value=Cat};

category_record_t(Cat) ->
    {_Scheme, Term} = occi_category:id(Cat),
    Location = hash_location(Term),
    #category{id=occi_category:id(Cat), location=Location, value=occi_category:location(Location, Cat)}.


%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
