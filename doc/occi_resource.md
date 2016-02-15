

# Module occi_resource #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#summary-1">summary/1</a></td><td></td></tr><tr><td valign="top"><a href="#summary-2">summary/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Id::<a href="uri.md#type-t">uri:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_uri, iolist()}`

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="uri.md#type-t">uri:t()</a> | string() | binary(), KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | string() | binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{unknown_category, term()} | {invalid_uri, iolist()}`

<a name="summary-1"></a>

### summary/1 ###

<pre><code>
summary(E::<a href="#type-t">t()</a>) -&gt; string()
</code></pre>
<br />

<a name="summary-2"></a>

### summary/2 ###

<pre><code>
summary(Summary::string() | binary(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

