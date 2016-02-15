

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#source-1">source/1</a></td><td></td></tr><tr><td valign="top"><a href="#target-1">target/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Id::<a href="uri.md#type-t">uri:t()</a> | string() | binary(), Src::<a href="uri.md#type-t">uri:t()</a> | string() | binary(), Target::<a href="uri.md#type-t">uri:t()</a> | string() | binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_uri, iolist()}`

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Id::<a href="uri.md#type-t">uri:t()</a> | string() | binary(), KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | string() | binary(), Src::<a href="uri.md#type-t">uri:t()</a> | string() | binary(), Target::<a href="uri.md#type-t">uri:t()</a> | string() | binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{unknown_category, term()} | {invalid_uri, iolist()}`

<a name="source-1"></a>

### source/1 ###

<pre><code>
source(E::<a href="#type-t">t()</a>) -&gt; <a href="uri.md#type-t">uri:t()</a>
</code></pre>
<br />

<a name="target-1"></a>

### target/1 ###

<pre><code>
target(E::<a href="#type-t">t()</a>) -&gt; <a href="uri.md#type-t">uri:t()</a>
</code></pre>
<br />

