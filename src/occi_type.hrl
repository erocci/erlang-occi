-ifndef(occi_type_hrl).
-define(occi_type_hrl, true).

-define(is_extension(X),    element(1, X) =:= extension).
-define(is_collection(X),   element(1, X) =:= collection).

-define(is_category_id(X), is_list(element(1, X)), is_list(element(2, X))). 

-define(is_category(X), element(1, X) =:= category; element(1, X) =:= kind; element(1, X) =:= mixin; element(1, X) =:= action).
-define(is_kind(X),     element(1, X) =:= kind).
-define(is_mixin(X),    element(1, X) =:= mixin).
-define(is_action(X),   element(1, X) =:= action).

-define(is_entity(X),   element(1, X) =:= entity; element(1, X) =:= resource; element(1, X) =:= link).
-define(is_resource(X), element(1, X) =:= resource).
-define(is_link(X),     element(1, X) =:= link).

-define(is_invoke(X), element(1, X) =:= invoke).

-endif.
