

# Module occi_link #
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
t() = <a href="occi_entity.md#type-t">occi_entity:t()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load-3">load/3</a></td><td>Load link from iolist.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Equivalent to <a href="#new-5"><tt>new(Id, KindId, Src, Target, occi_resource:kind(Target))</tt></a>.</td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td>Creates a new link.</td></tr><tr><td valign="top"><a href="#source-1">source/1</a></td><td></td></tr><tr><td valign="top"><a href="#target-1">target/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="load-3"></a>

### load/3 ###

<pre><code>
load(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist(), Ctx::<a href="#type-parse_ctx">parse_ctx()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Load link from iolist

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Id::<a href="uri.md#type-t">uri:t()</a>, KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Src::binary() | <a href="occi_resource.md#type-t">occi_resource:t()</a>, Target::binary() | <a href="occi_resource.md#type-t">occi_resource:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Equivalent to [`new(Id, KindId, Src, Target, occi_resource:kind(Target))`](#new-5).

<a name="new-6"></a>

### new/6 ###

<pre><code>
new(Id::<a href="uri.md#type-t">uri:t()</a>, KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Src::binary(), SrcKind::<a href="occi_category.md#type-id">occi_category:id()</a>, Target::binary(), TargetKind::<a href="occi_category.md#type-id">occi_category:id()</a> | undefined) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a new link

<a name="source-1"></a>

### source/1 ###

<pre><code>
source(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_uri.md#type-t">occi_uri:t()</a>
</code></pre>
<br />

<a name="target-1"></a>

### target/1 ###

<pre><code>
target(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_uri.md#type-t">occi_uri:t()</a>
</code></pre>
<br />

