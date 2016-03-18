%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_base_type).

-export([cast/2]).

-type spec() :: {enum, [atom()]}
	      | string
	      | integer
	      | float
	      | uri
	      | kind
	      | resource.

-type t() :: atom()
	   | string()
	   | integer()
	   | float().

-export_type([t/0, spec/0]).


%% @doc Return value casted as the specified OCCI base type.
%% Throw error if value can not be casted. Do only syntactic checking.
%%
%% @todo Better check of uri
%% @throws {invalid_value, spec(), any()}
%% @end
-spec cast(term(), spec()) -> t() | {error, term()}.
cast(V, {enum, Enum}) ->
    case cast_enum(V, Enum) of
	{ok, Atom} -> Atom;
	error -> throw({invalid_value, {enum, Enum}, V})
    end;

cast(V, string) when is_list(V) ->
    lists:flatten(V);

cast(V, string) when is_binary(V) ->
    binary_to_list(V);

cast(V, string) ->
    throw({invalid_value, string, V});

cast(V, integer) ->
    cast_integer(V);

cast(V, float) ->
    cast_float(V);

cast(V, uri) ->
    cast(V, string);

cast({Scheme, Term}=V, kind) when is_list(Scheme), is_list(Term) ->
    V;

cast(V, kind) ->
    try occi_category:parse_id(V) of
	Id -> Id
    catch throw:_ ->
	    throw({invalid_value, kind, V})
    end;

cast(V, resource) ->
    cast(V, uri).


%%%
%%% Internal
%%%
cast_enum(_Val, []) ->
    error;
cast_enum(Val, [ Head | Tail]) ->
    case atom_to_list(Head) of
        Val -> {ok, Head};
        _ -> cast_enum(Val, Tail)
    end.


cast_integer(X) when is_integer(X) ->
    X;

cast_integer(X) when is_binary(X) ->
    try binary_to_integer(X) of
        I -> I
    catch
        _:_ ->
            throw({invalid_value, integer, X})
    end;

cast_integer(X) when is_list(X) ->
    try list_to_integer(X) of
        I -> I
    catch
        _:_ ->
	    throw({invalid_value, integer, X})
    end;

cast_integer(X) ->
    throw({invalid_value, integer, X}).


cast_float(X) when is_float(X) ->
    X;

cast_float(X) when is_integer(X) ->
    X+0.0;

cast_float(X) when is_binary(X) ->
    try binary_to_float(X) of
        V -> V
    catch 
        _:_ ->
            try binary_to_integer(X) of
                V -> V+0.0
            catch
                _:_ -> 
                    throw({invalid_value, float, X})
            end
    end;

cast_float(X) when is_list(X) ->
    try list_to_float(X) of
        V -> V
    catch 
        _:_ ->
            try list_to_integer(X) of
                V -> V+0.0
            catch
                _:_ -> 
                    throw({invalid_value, float, X})
            end
    end;

cast_float(X) ->
    throw({invalid_value, float, X}).
