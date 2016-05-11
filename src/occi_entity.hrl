-ifndef(occi_entity_hrl).
-define(occi_entity_hrl, true).

-define(class,      1).
-define(id,         2).
-define(location,   3).
-define(kind,       4).
-define(mixins,     5).
-define(attributes, 6).
-define(values,     7).
-define(actions,    8).

-define(g(Key, C), maps:get(Key, element(?values, C))).
-define(s(Key, Value, C), setelement(?attributes, C, maps:update(Key, Value, element(?values, C)))).

-endif.
