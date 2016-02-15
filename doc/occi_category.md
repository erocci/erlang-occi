

# Module occi_category #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-class">class()</a> ###


<pre><code>
class() = kind | mixin | action
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = {Scheme::string(), Term::string()}
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = #{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_action-2">add_action/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_attribute-2">add_attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attribute-2">attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#class-1">class/1</a></td><td></td></tr><tr><td valign="top"><a href="#entity-0">entity/0</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#link_-0">link_/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_id-1">parse_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#resource-0">resource/0</a></td><td></td></tr><tr><td valign="top"><a href="#title-1">title/1</a></td><td></td></tr><tr><td valign="top"><a href="#title-2">title/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_action-2"></a>

### add_action/2 ###

<pre><code>
add_action(Action::<a href="occi_action.md#type-t">occi_action:t()</a>, C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="add_attribute-2"></a>

### add_attribute/2 ###

<pre><code>
add_attribute(Attr::<a href="occi_attribute.md#type-t">occi_attribute:t()</a>, C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="attribute-2"></a>

### attribute/2 ###

<pre><code>
attribute(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, C::<a href="#type-t">t()</a>) -&gt; <a href="occi_attribute.md#type-t">occi_attribute:t()</a>
</code></pre>
<br />

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(C::<a href="occi_category.md#type-t">occi_category:t()</a>) -&gt; #{}
</code></pre>
<br />

<a name="class-1"></a>

### class/1 ###

<pre><code>
class(C::<a href="#type-t">t()</a>) -&gt; <a href="#type-class">class()</a>
</code></pre>
<br />

<a name="entity-0"></a>

### entity/0 ###

`entity() -> any()`

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(C::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

<a name="link_-0"></a>

### link_/0 ###

`link_() -> any()`

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::string() | <a href="#type-id">id()</a>, Cls::<a href="#type-class">class()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_cid, term()}`

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Scheme::string(), Term::string(), Cls::<a href="#type-class">class()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="parse_id-1"></a>

### parse_id/1 ###

<pre><code>
parse_id(Id::string() | binary()) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

throws `{invalid_cid, term()}`

<a name="resource-0"></a>

### resource/0 ###

`resource() -> any()`

<a name="title-1"></a>

### title/1 ###

<pre><code>
title(C::<a href="#type-t">t()</a>) -&gt; string()
</code></pre>
<br />

<a name="title-2"></a>

### title/2 ###

<pre><code>
title(Title::string(), C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

