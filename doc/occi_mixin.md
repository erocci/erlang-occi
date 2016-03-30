

# Module occi_mixin #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


<pre><code>
t() = #mixin{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_apply-2">add_apply/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_depend-2">add_depend/2</a></td><td></td></tr><tr><td valign="top"><a href="#applies-1">applies/1</a></td><td></td></tr><tr><td valign="top"><a href="#depends-1">depends/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-3">load/3</a></td><td>Load mixin from iolist.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_apply-2"></a>

### add_apply/2 ###

<pre><code>
add_apply(Apply::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>, Mixin::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="add_depend-2"></a>

### add_depend/2 ###

<pre><code>
add_depend(Depend::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>, Mixin::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="applies-1"></a>

### applies/1 ###

<pre><code>
applies(M::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-id">occi_category:id()</a>]
</code></pre>
<br />

<a name="depends-1"></a>

### depends/1 ###

<pre><code>
depends(M::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-id">occi_category:id()</a>]
</code></pre>
<br />

<a name="load-3"></a>

### load/3 ###

<pre><code>
load(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Load mixin from iolist

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Scheme::binary(), Term::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

