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
load_path(MimeType, {path, Filename}) ->
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
entity() ->
    E = occi_category:entity(),
    #category{id=occi_category:id(E), extension=?core_scheme, value=E}.

resource() ->
    R = occi_category:resource(),
    #category{id=occi_category:id(R), extension=?core_scheme, value=R}.

link_() ->
    L = occi_category:link_(),
    #category{id=occi_category:id(L), extension=?core_scheme, value=L}.

core_categories() ->
    Op = fun() ->
		 ok = mnesia:write(category, entity(), write),
		 ok = mnesia:write(category, resource(), write),
		 ok = mnesia:write(category, link_(), write)
	 end,
    {atomic, ok} = mnesia:transaction(Op),
    ok.


load_extension(Extension) ->
    ok = load_imports(occi_extension:imports(Extension)).


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
	    load_extension(Imports)
    end.


parser({<<"application">>, <<"xml">>, []}) -> occi_parser_xml;
parser({<<"application">>, <<"occi+xml">>, []}) -> occi_parser_xml;
parser({<<"application">>, <<"json">>, []}) -> occi_parser_json;
parser({<<"application">>, <<"occi+json">>, []}) -> occi_parser_json.

%%%
%%% eunit
%%%
-ifdef(TEST).

-endif.
