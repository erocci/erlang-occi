-ifndef(occi_category_hrl).
-define(occi_category_hrl, true).

-define(g(Key, C), maps:get(Key, element(3, C))).
-define(s(Key, Value, C), setelement(3, C, maps:update(Key, Value, element(3, C)))).

-endif.
