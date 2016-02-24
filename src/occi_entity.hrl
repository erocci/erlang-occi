-ifndef(occi_entity_hrl).
-define(occi_entity_hrl, true).

-define(class,      1).
-define(id,         2).
-define(kind,       3).
-define(mixins,     4).
-define(attributes, 5).

-define(g(Key, C), maps:get(Key, element(?attributes, C))).
-define(s(Key, Value, C), setelement(?attributes, C, maps:update(Key, Value, element(?attributes, C)))).

-endif.
