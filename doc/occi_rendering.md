

# Module occi_rendering #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Provide common functions for rendering/parsing OCCI types.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load_collection-3">load_collection/3</a></td><td>Load the specified OCCI collection
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}.</td></tr><tr><td valign="top"><a href="#load_entity-4">load_entity/4</a></td><td>Load the specified OCCI entity (or sub-type thereof from an iolist()
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}.</td></tr><tr><td valign="top"><a href="#load_invoke-3">load_invoke/3</a></td><td>Load the specified OCCI action invocation
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}.</td></tr><tr><td valign="top"><a href="#load_model-4">load_model/4</a></td><td>Load the specified OCCI category or extension from an iolist()
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="load_collection-3"></a>

### load_collection/3 ###

<pre><code>
load_collection(MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_collection.md#type-t">occi_collection:t()</a>
</code></pre>
<br />

throws `{parse_error, [occi_parser:errors()](occi_parser.md#type-errors)} | {unknown_mimetype, term()}`

Load the specified OCCI collection
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}


<a name="load_entity-4"></a>

### load_entity/4 ###

<pre><code>
load_entity(Type::<a href="occi_type.md#type-name">occi_type:name()</a>, MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

throws `{parse_error, [occi_parser:errors()](occi_parser.md#type-errors)} | {unknown_mimetype, term()}`

Load the specified OCCI entity (or sub-type thereof from an iolist()
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}


<a name="load_invoke-3"></a>

### load_invoke/3 ###

<pre><code>
load_invoke(MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_invoke.md#type-t">occi_invoke:t()</a>
</code></pre>
<br />

throws `{parse_error, [occi_parser:errors()](occi_parser.md#type-errors)} | {unknown_mimetype, term()}`

Load the specified OCCI action invocation
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}


<a name="load_model-4"></a>

### load_model/4 ###

<pre><code>
load_model(Type::<a href="occi_type.md#type-name">occi_type:name()</a>, MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

throws `{parse_error, [occi_parser:errors()](occi_parser.md#type-errors)} | {unknown_mimetype, term()}`

Load the specified OCCI category or extension from an iolist()
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}


<a name="render-3"></a>

### render/3 ###

<pre><code>
render(MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, T::<a href="occi_type.md#type-t">occi_type:t()</a>, Ctx::<a href="#type-render_ctx">render_ctx()</a>) -&gt; iolist()
</code></pre>
<br />

