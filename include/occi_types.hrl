-ifndef(occi_types_hrl).
-define(occi_types_hrl, 1).

-define(is_extension(X),    element(1, X) =:= extension).
-define(is_categories(X),   is_list(X)).
-define(is_collection(X),   element(1, X) =:= collection).

-define(is_category_id(X), is_binary(element(1, X)) andalso is_binary(element(2, X))). 

-define(is_category(X), element(1, X) =:= category orelse element(1, X) =:= kind orelse element(1, X) =:= mixin orelse element(1, X) =:= action).
-define(is_kind(X),     element(1, X) =:= kind).
-define(is_mixin(X),    element(1, X) =:= mixin).
-define(is_action(X),   element(1, X) =:= action).

-define(is_entity(X),   element(1, X) =:= entity orelse element(1, X) =:= resource orelse element(1, X) =:= link).
-define(is_resource(X), element(1, X) =:= resource).
-define(is_link(X),     element(1, X) =:= link).

-define(is_invoke(X), element(1, X) =:= invoke).

-define(is_uri(X), element(1, X) =:= uri).

-define(is_occi_mod(X), occi_extension =:= X orelse occi_collection =:= X orelse occi_category =:= X orelse occi_kind =:= X orelse occi_mixin =:= X orelse occi_action =:= X orelse occi_attribute =:= X orelse occi_entity =:= X orelse occi_resource =:= X orelse occi_link =:= X orelse occi_invoke =:= X).

-endif.
