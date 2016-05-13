%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Apr 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_ctx).

-export([new/1,
	 new/2,
	 model/0,
	 model/1,
	 client/1]).

%% @doc Check type:
%% <ul>
%%   <li>server: immutable attributes can be changed, required ones must be set</li>
%%   <li>client: immutable attributes can not be set</li>
%%   <li>internal: for entity sub-types. Do not check if all required attributes are set (for instance in link constructor)</li>
%% </ul>
%% @end
-type validation() :: server | client | internal | model.
-define(is_validation(X), server =:= X orelse client =:= X orelse internal =:= X orelse model =:= X).

-opaque t() :: maps:map().

-export_type([t/0]).


%% @equiv new(internal, Url)
%% @end
new(Url) ->
    new(internal, Url).


%% @doc Creates a rendering context
%% @end
-spec new(Validation :: validation(), Url :: occi_uri:t() | binary() | undefined) -> t().
new(Validation, undefined) when ?is_validation(Validation) ->
    #{ valid => Validation };

new(Validation, Url) when is_binary(Url), ?is_validation(Validation) ->
    #{ valid => Validation, url => occi_uri:from_string(Url) };

new(Validation, Url) when ?is_validation(Validation) ->
    #{ valid => Validation, url => Url }.


%% @equiv new(client, Url)
%% @end
client(Url) ->
    new(client, Url).


%% @equiv new(model, undefined)
%% @end
model() ->
    new(model, undefined).


%% @equiv new(model, Url)
%% @end
model(Url) ->
    new(model, Url).
