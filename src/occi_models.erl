%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_models).

-export([init/0]).
-on_load(init/0).

-export([load_path/2,
	 load/2,
	 category/1,
	 add_category/2,
	 attribute/2,
	 attributes/1]).

%% Defines mimetype as in cowlib
-type mimetype() :: {Type :: binary(), SubType :: binary(), Options :: list()}.

%% internal
-define(core_scheme, "http://schemas.ogf.org/occi/core#").

-record category, {id        :: occi_category:id(),
		   extension :: occi_extension:id(),
		   value     :: occi_category:t()}.

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


%% @throws enoent | eacces | eisdir | enotdir | enomem
-spec load_path(MimeType :: mimetype(), file:filename_all()) -> ok.
load_path(MimeType, Filename) ->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    load(MimeType, Bin);
	{error, Reason} ->
	    throw(Reason)
    end.

%% @doc Load a model from an iolist()
%% Mimetype must be given as {Type :: binary(), SubType :: binary(), []}
%%
%% Supported types are:
%% <ul>
%%   <li>{<<"application">>, <<"xml">>, []}</li>
%%   <li>{<<"application">>, <<"occi+xml">>, []}</li>
%%   <li>{<<"application">>, <<"json">>, []}</li>
%%   <li>{<<"application">>, <<"occi+json">>, []}</li>
%% </ul>
%% @throws {unknown_mimetype, term()}
%% @throws {parse_error, occi_parser:errors()}
%% @throws {unknown_extension, occi_extension:id()}
load(MimeType, Bin) when is_list(Bin); is_binary(Bin) ->
    case parser(MimeType) of
	undefined -> 
	    throw({unknown_mimetype, MimeType});
	Mod -> 
	    Ext = Mod:parse_extension(Bin),
	    load_extension(Ext)
    end.
	    

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


load_extension(E) ->
    ok = load_imports(occi_extension:imports(E)),
    ok = load_categories(occi_extension:scheme(E), occi_extension:kinds(E)),
    ok = load_categories(occi_extension:scheme(E), occi_extension:mixins(E)).


%% Check extension exists in the database, throw error if not
load_imports([]) ->
    ok;

load_imports([ Scheme | Imports ]) ->
    Op = fun() ->
		 mnesia:match_object({category, '_', Scheme, '_'})
	 end,
    case mnesia:transaction(Op) of
	{atomic, []} ->
	    throw({unknown_extension, Scheme});
	{atomic, _} ->
	    load_imports(Imports)
    end.


load_categories(_Scheme, []) ->
    ok;

load_categories(Scheme, [ Cat | Categories ]) ->
    ok = add_category(Scheme, Cat),
    load_categories(Scheme, Categories).


parser({<<"application">>, <<"xml">>, []}) -> occi_parser_xml;
parser({<<"application">>, <<"occi+xml">>, []}) -> occi_parser_xml;
parser({<<"application">>, <<"json">>, []}) -> occi_parser_json;
parser({<<"application">>, <<"occi+json">>, []}) -> occi_parser_json.

%%%
%%% eunit
%%%
-ifdef(TEST).
load_path_test_() ->
    load_path({<<"application">>, <<"xml">>, []}, filename:join([priv_dir(), "schemas", "occi-infrastructure.xml"])),
    [
     ?_assertMatch(kind, 
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "compute"}))),
     ?_assertMatch(kind,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "network"}))),
     ?_assertMatch(kind,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "storage"}))),
     ?_assertMatch(kind,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "storagelink"}))),
     ?_assertMatch(kind,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "networkinterface"}))),
     ?_assertMatch(mixin,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure/network#", "ipnetwork"}))),
     ?_assertMatch(mixin, 
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure/networkinterface#", "ipnetworkinterface"}))),
     ?_assertMatch(mixin,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "os_tpl"}))),
     ?_assertMatch(mixin,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "resource_tpl"}))),
     ?_assertMatch(mixin,
		   occi_category:class(category({"http://schemas.ogf.org/occi/infrastructure#", "large"}))),
     ?_assertMatch(mixin,
		   occi_category:class(category({"http://occi.example.org/occi/infrastructure/os_tpl#", "debian6"})))
    ].

priv_dir() ->
    filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]).

-endif.
