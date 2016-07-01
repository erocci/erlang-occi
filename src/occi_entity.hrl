-ifndef(occi_entity_hrl).
-define(occi_entity_hrl, true).

-define(class,      1).
-define(location,   2).
-define(kind,       3).
-define(mixins,     4).
-define(attributes, 5).
-define(values,     6).
-define(actions,    7).

-define(g(Key, C), maps:get(Key, element(?values, C))).
-define(s(Key, Value, C), setelement(?attributes, C, maps:update(Key, Value, element(?values, C)))).

-endif.
