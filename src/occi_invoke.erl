%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Implements action invocation
%%%
%%% @end
%%% Created : 25 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_invoke).

-include("occi_rendering.hrl").
-include("occi_type.hrl").

-export([new/2,
	 id/1,
	 title/1,
	 title/2,
	 attributes/1]).

-export([load/3,
	 render/3]).

-record(invoke, {id         :: occi_category:id(),
		 title      :: string(),
		 attributes :: maps:map()}).

-type t() :: #invoke{}.

%% @doc Creates a new action invocation
%% @end
-spec new(occi_category:id(), maps:map()) -> t().
new(Id, Attributes) when ?is_category_id(Id), is_map(Attributes) ->
    #invoke{id=Id, attributes=Attributes}.


%% @doc Return action id
%% @end
-spec id(t()) -> occi_category:id().
id(#invoke{ id=Id }) ->
    Id.


%% @doc Return action title
%% @end
-spec title(t()) -> string().
title(#invoke{ title=Title }) ->
    Title.


%% @doc Set action title
%% @end
-spec title(string() | binary(), t()) -> t().
title(Title, I) when is_binary(Title) ->
    title(binary_to_list(Title), I);

title(Title, #invoke{}=I) ->
    I#invoke{ title=Title }.


%% @doc Get invocation attributes
%% @end
-spec attributes(t()) -> maps:map().
attributes(#invoke{attributes=Attributes}) ->
    Attributes.


%% @doc Load an action invocation from iolist
%% @end
load(Mimetype, Bin, Ctx) -> 
    occi_rendering:load_invoke(Mimetype, Bin, Ctx).


%% @doc Render action invocation into given mimetype
%% @end
-spec render(occi_utils:mimetype(), t(), render_ctx()) -> iolist().
render(Mimetype, I, Ctx) ->
    occi_rendering:render(Mimetype, I, Ctx).
