

# Module occi_rendering #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Provide common functions for rendering/parsing OCCI types.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-ast">ast()</a> ###


<pre><code>
ast() = #{<a href="#type-ast_key">ast_key()</a> =&gt; <a href="#type-ast_value">ast_value()</a>}
</code></pre>




### <a name="type-ast_key">ast_key()</a> ###


<pre><code>
ast_key() = categories | attributes | actions | id | links | summary | title | action | source | target | parent | location | depends | applies
</code></pre>




### <a name="type-ast_link_end">ast_link_end()</a> ###


<pre><code>
ast_link_end() = #{location | kind =&gt; binary()}
</code></pre>




### <a name="type-ast_value">ast_value()</a> ###


<pre><code>
ast_value() = binary() | list() | <a href="maps.md#type-map">maps:map()</a> | <a href="#type-ast_link_end">ast_link_end()</a>
</code></pre>




### <a name="type-errors">errors()</a> ###


<pre><code>
errors() = {parse_error, term()} | {unknown_mimetype, term()} | {badkey, atom()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse an OCCI object and returns it as a map.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

throws `[errors()](#type-errors)`

Parse an OCCI object and returns it as a map.

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}

* {<<"text">>, <<"plain">>, []}

* {<<"text">>, <<"occi">>, []}


<a name="render-3"></a>

### render/3 ###

<pre><code>
render(MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, T::<a href="occi_type.md#type-t">occi_type:t()</a>, Ctx::<a href="occi_ctx.md#type-t">occi_ctx:t()</a>) -&gt; iolist()
</code></pre>
<br />

