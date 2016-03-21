-ifndef(occi_uri_hrl).
-define(occi_uri_hrl, true).

-define(uri_scheme(S, X), {uri, S, _, _, _, _, _, _, _} =:= X).
-define(uri_host(H, X),   {uri, _, _, H, _, _, _, _, _} =:= X).
-define(uri_path(P, X),   {uri, _, _, _, _, P, _, _, _} =:= X).

-define(is_uri(X), element(1, X) =:= uri).

-endif.
