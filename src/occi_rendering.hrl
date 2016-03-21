-ifndef(occi_rendering_hrl).
-define(occi_rendering_hrl, true).

%% @doc Check type:
%% <ul>
%%   <li>server: immutable attributes can be changed, required ones must be set</li>
%%   <li>client: immutable attributes can not be set</li>
%%   <li>internal: for entity sub-types. Do not check if all required attributes are set (for instance in link constructor)</li>
%% </ul>
%% @end
-type validation() :: server | client | internal | model.

-record(parse_ctx, {
	  valid :: validation(),
	  url   :: uri:t() | undefined
	 }).


-type parse_ctx() :: #parse_ctx{}.
-type render_ctx() :: uri:t().

-endif.
