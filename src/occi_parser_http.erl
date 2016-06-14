%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_http).

-export([parse/1]).

-define(is_uppercase(C), C >= 65, C =< 90).
-define(is_lowercase(C), C >= 97, C =< 122).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Parse HTTP-style headers and return dictionary
%% @end
-spec parse(binary() | proplists:proplist()) -> orddict:orddict().
parse(Bin) when is_binary(Bin) ->
    p_headers(Bin, orddict:new());

parse(Headers) when is_list(Headers) ->
    p_proplist(Headers, orddict:new()).


%%%
%%% Priv
%%%
p_proplist([], Acc) ->
    reverse(Acc);

p_proplist([ {Name, Bin} | Tail ], Acc) ->
    {Key, Values, _} = p_header_name3(Bin, Name),
    p_proplist(Tail, add_header_values(Key, Values, Acc)).


p_headers(<<>>, Acc) ->
    reverse(Acc);

p_headers(<< $\r, $\n, Rest/binary >>, Acc) ->
    p_headers(Rest, Acc);

p_headers(<< $\r, Rest/binary >>, Acc) ->
    p_headers(Rest, Acc);

p_headers(<< $\n, Rest/binary >>, Acc) ->
    p_headers(Rest, Acc);

p_headers(<< $\s, Rest/binary >>, Acc) ->
    p_headers(Rest, Acc);

p_headers(Bin, Acc) ->
    {Name, Values, Rest} = p_header(Bin),
    p_headers(Rest, add_header_values(Name, Values, Acc)).


reverse(H) ->
    orddict:fold(fun (Key, Values, Acc) ->
                         orddict:store(Key, lists:reverse(Values), Acc)
                 end, orddict:new(), H).


p_header(<< C, Rest/binary >>) when ?is_uppercase(C) ->
    p_header_name(Rest, << (C+32) >>);

p_header(<< C, Rest/binary >>) when ?is_lowercase(C) ->
    p_header_name(Rest, << C >>);

p_header(<< C, _Rest/binary >>) ->
    throw({parse_error, {header_name, C}}).


p_header_name(<<>>, _Acc) ->
    throw({parse_error, {header_name, <<>>}});

p_header_name(<< $\s, Rest/binary >>, Acc) ->
    p_header_name2(Rest, Acc);

p_header_name(<< $:, Rest/binary >>, Acc) ->
    p_header_name3(eat_ws(Rest), Acc);

p_header_name(<< C, Rest/binary >>, Acc) when ?is_uppercase(C) ->
    p_header_name(Rest, << Acc/binary, (C+32) >>);

p_header_name(<< C, Rest/binary >>, Acc) ->
    p_header_name(Rest, << Acc/binary, C >>).


p_header_name2(<<>>, _Acc) ->
    throw({parse_error, {header_name, <<>>}});

p_header_name2(<< $\s, Rest/binary >>, Acc) ->
    p_header_name2(Rest, Acc);

p_header_name2(<< $:, Rest/binary >>, Name) ->
    p_header_name3(eat_ws(Rest), Name);

p_header_name2(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {header_name, C}}).


p_header_name3(Bin, <<"category">>)         -> p_header_value(Bin, 'category');

p_header_name3(Bin, <<"x-occi-location">>)  -> p_header_value(Bin, 'x-occi-location');

p_header_name3(Bin, <<"x-occi-attribute">>) -> p_header_value(Bin, 'x-occi-attribute');

p_header_name3(Bin, <<"link">>)             -> p_header_value(Bin, 'link');

p_header_name3(Bin, <<"location">>)         -> p_header_value(Bin, 'location');

p_header_name3(Bin, Name)                   -> p_header_value(Bin, Name).


p_header_value(<< $\r, $\n, Rest/binary >>, Name) ->
    {Name, [], Rest};

p_header_value(<< $\r, Rest/binary >>, Name) ->
    {Name, [], Rest};

p_header_value(<< $\n, Rest/binary >>, Name) ->
    {Name, [], Rest};

p_header_value(<< $,, Rest/binary >>, Name) ->
    p_header_value(eat_ws(Rest), Name);

p_header_value(<< $", Rest/binary >>, Name) ->
    p_header_string(Rest, Name, [], << $" >>);

p_header_value(<< C, Rest/binary >>, Name) ->
    p_header_value2(Rest, Name, [], << C >>).


p_header_value2(<<>>, Name, Values, Acc) ->
    {Name, [ Acc | Values ], <<>>};

p_header_value2(<< $\r, $\n, Rest/binary >>, Name, Values, Acc) ->
    {Name, [ Acc | Values ], Rest};

p_header_value2(<< $\r, Rest/binary >>, Name, Values, Acc) ->
    {Name, [ Acc | Values ], Rest};

p_header_value2(<< $\n, Rest/binary >>, Name, Values, Acc) ->
    {Name, [ Acc | Values ], Rest};

p_header_value2(<< $,, Rest/binary >>, Name, Values, Acc) ->
    p_header_value2(eat_ws(Rest), Name, [ Acc | Values ], <<>>);

p_header_value2(<< $", Rest/binary >>, Name, Values, Acc) ->
    p_header_string(Rest, Name, Values, << Acc/binary, $" >>);

p_header_value2(<< C, Rest/binary >>, Name, Values, Acc) ->
    p_header_value2(Rest, Name, Values, << Acc/binary, C >>).


p_header_string(<<>>, _Name, _Values, _Acc) ->
    throw({parse_error, {header, <<>>}});

p_header_string(<< $\r, $\n, _Rest/binary >>, _Name, _Values, _Acc) ->
    throw({parse_error, {header, eol}});

p_header_string(<< $\r, _Rest/binary >>, _Name, _Values, _Acc) ->
    throw({parse_error, {header, eol}});

p_header_string(<< $\n, _Rest/binary >>, _Name, _Values, _Acc) ->
    throw({parse_error, {header, eol}});

p_header_string(<< $\\, $", Rest/binary >>, Name, Values, Acc) ->
    p_header_string(Rest, Name, Values, << Acc/binary, $\\, $" >>);

p_header_string(<< $", Rest/binary >>, Name, Values, Acc) ->
    p_header_value2(Rest, Name, Values, << Acc/binary, $" >>);

p_header_string(<< C, Rest/binary >>, Name, Values, Acc) ->
    p_header_string(Rest, Name, Values, << Acc/binary, C >>).


add_header_values(Name, Values, Acc) ->
    Values0 = case orddict:find(Name, Acc) of
		  {ok, V} -> V;
		  error -> []
	      end,
    orddict:store(Name, Values ++ Values0, Acc).


eat_ws(<< $\s, Rest/binary >>) ->
    eat_ws(Rest);

eat_ws(Rest) ->
    Rest.

%%%
%%% eunit
%%%
-ifdef(TEST).

parse_test_() ->
    [
     ?_assertMatch({ok, [<<"\"my, foolish, thing\"">>, <<"another">>]}, 
		   orddict:find(<<"h1">>, parse(<<"h1:   \"my, foolish, thing\",   another">>)))
    ].
-endif.
