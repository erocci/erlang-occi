

# Module occi_parser_text #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_collection-2">parse_collection/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_entity-3">parse_entity/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_invoke-2">parse_invoke/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_model-3">parse_model/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_collection-2"></a>

### parse_collection/2 ###

<pre><code>
parse_collection(Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_collection.md#type-t">occi_collection:t()</a>
</code></pre>
<br />

<a name="parse_entity-3"></a>

### parse_entity/3 ###

<pre><code>
parse_entity(Type::entity | resource | link, Bin::binary(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

<a name="parse_invoke-2"></a>

### parse_invoke/2 ###

<pre><code>
parse_invoke(Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_invoke.md#type-t">occi_invoke:t()</a>
</code></pre>
<br />

<a name="parse_model-3"></a>

### parse_model/3 ###

<pre><code>
parse_model(Type::extension | kind | mixin | action, Bin::binary(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

