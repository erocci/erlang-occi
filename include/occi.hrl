-ifndef(occi_hrl).
-define(occi_hrl, true).

-define(entity_kind_id, {<<"http://schemas.ogf.org/occi/core#">>, <<"entity">>}).
-define(resource_kind_id, {<<"http://schemas.ogf.org/occi/core#">>, <<"resource">>}).
-define(link_kind_id, {<<"http://schemas.ogf.org/occi/core#">>, <<"link">>}).


-define(mimetype_plain,    {<<"text">>, <<"occi+plain">>, []}).
-define(mimetype_plain(V), {<<"text">>, <<"occi+plain">>, V}).
-define(mimetype_occi,     {<<"text">>, <<"occi">>, []}).
-define(mimetype_occi(V),  {<<"text">>, <<"occi">>, V}).
-define(mimetype_uri,      {<<"text">>, <<"uri-list">>, []}).
-define(mimetype_uri(V),   {<<"text">>, <<"uri-list">>, V}).
-define(mimetype_xml,      {<<"application">>, <<"occi+xml">>, []}).
-define(mimetype_xml(V),   {<<"application">>, <<"occi+xml">>, V}).
-define(mimetype_json,     {<<"application">>, <<"occi+json">>, []}).
-define(mimetype_json(V),  {<<"application">>, <<"occi+json">>, V}).

-endif.
