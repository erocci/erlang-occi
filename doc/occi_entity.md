

# Module occi_entity #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Represents an OCCI entity.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
Uses maps for internal representation.
Type checking is achieved with check/1. When setting an attribute,
only attribute existence is checked.

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


<pre><code>
t() = #entity{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_mixin-2">add_mixin/2</a></td><td>Add a mixin to this entity
If an attribute is already defined, this mixin's definition take precedence over
the previous one.</td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td>Return key-value attributes map.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#kind-1">kind/1</a></td><td></td></tr><tr><td valign="top"><a href="#mixins-1">mixins/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#rm_mixin-2">rm_mixin/2</a></td><td>Unassociate mixin from this entity
Attributes only defined by this mixin are removed.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#title-1">title/1</a></td><td></td></tr><tr><td valign="top"><a href="#title-2">title/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_mixin-2"></a>

### add_mixin/2 ###

<pre><code>
add_mixin(MixinId::<a href="occi_category.md#type-id">occi_category:id()</a> | string() | binary(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_category, [occi_category:id()](occi_category.md#type-id)}`

Add a mixin to this entity
If an attribute is already defined, this mixin's definition take precedence over
the previous one.

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(E::<a href="#type-t">t()</a>) -&gt; #{}
</code></pre>
<br />

Return key-value attributes map

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="occi_attribute.md#type-value">occi_attribute:value()</a> | undefined
</code></pre>
<br />

throws `{invalid_key, [occi_attribute:key()](occi_attribute.md#type-key)}`

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(E::<a href="#type-t">t()</a>) -&gt; <a href="uri.md#type-t">uri:t()</a>
</code></pre>
<br />

<a name="kind-1"></a>

### kind/1 ###

<pre><code>
kind(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_kind.md#type-id">occi_kind:id()</a>
</code></pre>
<br />

<a name="mixins-1"></a>

### mixins/1 ###

<pre><code>
mixins(E::<a href="#type-t">t()</a>) -&gt; [<a href="occi_mixin.md#type-id">occi_mixin:id()</a>]
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Id::string()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{unknown_category, term()}`

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::string(), KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | string() | binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{unknown_category, term()}`

<a name="rm_mixin-2"></a>

### rm_mixin/2 ###

<pre><code>
rm_mixin(MixinId::<a href="occi_category.md#type-id">occi_category:id()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Unassociate mixin from this entity
Attributes only defined by this mixin are removed

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, Value::<a href="occi_attribute.md#type-value">occi_attribute:value()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="occi_attribute.md#type-value">occi_attribute:value()</a>
</code></pre>
<br />

throws `{invalid_key, [occi_attribute:key()](occi_attribute.md#type-key)} | {invalid_value, [occi_base_type:spec()](occi_base_type.md#type-spec), term()}`

<a name="title-1"></a>

### title/1 ###

<pre><code>
title(E::<a href="#type-t">t()</a>) -&gt; string()
</code></pre>
<br />

<a name="title-2"></a>

### title/2 ###

<pre><code>
title(Title::string() | binary(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

