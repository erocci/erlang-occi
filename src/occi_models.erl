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

-behaviour(gen_server).

-export([start_link/0,
	 init_mnesia/0]).

-export([import/1,
	 categories/0,
	 category/1,
	 kind/1,
	 kind/2,
	 action/1,
	 location/1,
	 add_category/1,
	 rm_category/1,
	 attribute/2,
	 attributes/1]).

%% gen_server callbacks
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3]).

%% internal
-define(core_scheme, <<"http://schemas.ogf.org/occi/core#">>).
-define(erocci_scheme, <<"http://erocci.ow2.org/occi#">>).

-define(REC, ?MODULE).
-record ?REC, {id        :: occi_category:id(),
	       location  :: binary(),
	       value     :: occi_category:t()}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Import an extension into the model
%%
%% @end
-spec import(occi_extension:t()) -> {ok, [occi_category:t()]} | {error, term()}.
import(E) ->
    Categories0 = occi_extension:kinds(E) ++ occi_extension:mixins(E),
    case load_imports(occi_extension:imports(E), Categories0) of
	{ok, Categories1} ->
	    ok = lists:foreach(fun (Category) -> 
				       ?debug("Add category ~p", [occi_category:id(Category)]),
				       _ = add_category(Category),
				       lists:foreach(fun (Action) ->
							     _ = add_category(Action)
						     end, occi_category:actions(Category))
			       end, Categories0),
	    {ok, Categories1};
	{error, _}=Err ->
	    Err
    end.


%% @doc Return the list of categories
%%
%% @end
-spec categories() -> [occi_category:t()].
categories() ->
    case mnesia:transaction(fun categories_t/0) of
	{aborted, Err} -> throw(Err);
	{atomic, Categories} -> Categories
    end.


%% @doc Return a kind
%% @throws {unknown_category, occi_category:id()}
%% @end
-spec kind(occi_category:id() | binary()) -> occi_category:id().
kind(KindId) when is_binary(KindId) ->
    kind(occi_category:parse_id(KindId));

kind(KindId) when ?is_category_id(KindId) ->
    case category(KindId) of
	undefined ->
	    throw({unknown_category, KindId});
	Category ->
	    case occi_category:class(Category) of
		kind -> Category;
		_ -> throw({unknown_category, KindId})
	    end
    end.


%% @doc Return a kind, checking it has specified parent 
%% @throws {unknown_category, occi_category:id()} | {invalid_kind, occi_category:id()}
%% @end
-spec kind(link | resource, occi_category:id()) -> occi_category:t().
kind(Parent, KindId) when ?is_category_id(KindId) ->
    Kind = kind(KindId),
    case occi_kind:has_parent(Parent, Kind) of
	true -> Kind;
	false -> throw({invalid_kind, KindId})
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
		  mnesia:match_object(#?REC{_='_', location=Path})
	  end,		  
    case mnesia:transaction(Fun) of
	{aborted, Err} -> throw(Err);
	{atomic, []} -> undefined;
	{atomic, [#?REC{value=Category}]} -> Category
    end.


%% @doc Return a category
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


%% @doc Add category to model and returns the stored category,
%% eventually with location set
%% @end
-spec add_category(occi_category:t()) -> occi_category:t().
add_category(Cat) when ?is_category(Cat) ->
    case mnesia:transaction(fun () -> add_category_t(Cat) end) of
	{atomic, Cat1} -> Cat1;
	{aborted, Err} -> throw(Err)
    end.


-spec rm_category(occi_category:id()) -> ok.
rm_category(Id) when ?is_category_id(Id) ->
    Fun = fun () ->
		  mnesia:delete({?REC, Id})
	  end,
    case mnesia:transaction(Fun) of
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


%%% gen_server callbacks
init([]) ->
    {ok, undefined}.


init_mnesia() ->
    case mnesia:create_table(?REC, [{ram_copies, nodes()}, {attributes, record_info(fields, ?REC)}]) of
	{atomic, ok} -> 
	    init_core();
	{aborted, {already_exists, ?REC}} -> 
	    init_core();
	{aborted, _} = Err -> 
	    {stop, Err}
    end.


init_core() ->
    case load_imports([?core_scheme, ?erocci_scheme], []) of
	{ok, _Categories} -> ok;
	{error, _}=Err -> Err
    end.


handle_info(_Info, S) ->
    {noreply, S}.


handle_call(_Call, _From, S) ->
    {reply, ok, S}.


handle_cast(_Evt, S) ->
    {noreply, S}.


terminate(_Reason, _S) ->
    ok.


code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%
%% Priv
%%
%% Check extension exists in the database, throw error if not
load_imports([], Categories) ->
    {ok, Categories};

load_imports([ Scheme | Imports ], Acc) ->
    ?debug("Import extension: ~s", [Scheme]),
    case dl_schema(Scheme) of
	{ok, Path} ->
	    case import(occi_rendering:parse_file(Path, occi_extension)) of
		{ok, Categories} ->
		    load_imports(Imports, Acc ++ Categories);
		{error, Err} ->
		    throw(Err)
	    end;
	{error, Err} ->
	    throw({import, Err})
    end.


dl_schema(Scheme) ->
    S = http_uri:encode(binary_to_list(Scheme)),
    Urls = [baseurl() ++ "/" ++ http_uri:encode(S) ++ ".xml"],
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
    Prefix = occi_utils:normalize(application:get_env(occi, collections_prefix, <<"/">>)),
    Loc = filename:join([Prefix, Term]),
    hash_location2(exists_location(Loc), Loc, 0).


hash_location2(false, Loc, _I) ->
    Loc;

hash_location2(true, Loc, I) ->
    Loc2 = << Loc/binary, (integer_to_binary(I))/binary >>,
    hash_location2(exists_location(Loc2), Loc2, I+1).


exists_location(Loc) ->
    case mnesia:dirty_match_object(#?REC{_='_', location=Loc}) of
	[] ->  false;
	_ -> true
    end.


%%%
%%% Requests functions
%%%
categories_t() ->
    mnesia:foldl(fun (#?REC{value=Cat}, Acc) ->
			 [ resolve_t(occi_category:class(Cat), Cat) | Acc ]
		 end, [], ?REC).


-spec category_t(occi_category:id()) -> occi_category:t() | undefined.
category_t(Id) ->
    case mnesia:read(?REC, Id, read) of
	[] -> 
	    undefined;
	[#?REC{value=C}] -> 
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
    		    Parents = [ParentId | occi_kind:parents(resolve_t(occi_category:class(Parent), Parent)) ],
    		    occi_kind:parents(Parents, C)
    	    end
    end;

resolve_t(mixin, C) ->
    C;

resolve_t(action, C) ->
    C.


add_category_t(Cat) ->
    case mnesia:read(?REC, occi_category:id(Cat)) of
	[] -> 
	    Cat1 = case occi_category:location(Cat) of
		       undefined -> gen_location(Cat);
		       _ -> Cat
		   end,
	    ok = mnesia:write(category_record_t(Cat1)),
	    Cat1;
	_ ->
	    %% Ignore duplicate category
	    Cat
    end.


category_record_t(Cat) when ?is_action(Cat) ->
    #?REC{id=occi_category:id(Cat), location=undefined, value=Cat};

category_record_t(Cat) ->
    Location = occi_category:location(Cat),
    #?REC{id=occi_category:id(Cat), location=Location, value=Cat}.


gen_location(Cat) when ?is_action(Cat) ->
    Cat;

gen_location(Cat) ->
    {_Scheme, Term} = occi_category:id(Cat),
    Location = hash_location(Term),
    occi_category:location(Location, Cat).

%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
