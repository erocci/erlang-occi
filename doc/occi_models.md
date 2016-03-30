

# Module occi_models #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

The module is used to access the full model of an OCCI endpoint,
ie, more or less extensions with resolved imports and location associated
to each kind or mixin.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-1">action/1</a></td><td>Return an action definition.</td></tr><tr><td valign="top"><a href="#add_category-1">add_category/1</a></td><td></td></tr><tr><td valign="top"><a href="#attribute-2">attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#categories-0">categories/0</a></td><td>Return the list of categories
Categories references (parent, depends, etc) are not resolved.</td></tr><tr><td valign="top"><a href="#category-1">category/1</a></td><td>Return a category
Category references (parent, depend, etc) is resolved:
attributes and actions from references are merged into the resulting category.</td></tr><tr><td valign="top"><a href="#import-1">import/1</a></td><td>Import an extension into the model.</td></tr><tr><td valign="top"><a href="#kind-2">kind/2</a></td><td>Return a kind, checking it has specified parent.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-1"></a>

### action/1 ###

<pre><code>
action(ActionId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_action.md#type-t">occi_action:t()</a>
</code></pre>
<br />

Return an action definition

<a name="add_category-1"></a>

### add_category/1 ###

<pre><code>
add_category(Cat::<a href="occi_category.md#type-t">occi_category:t()</a>) -&gt; ok
</code></pre>
<br />

<a name="attribute-2"></a>

### attribute/2 ###

<pre><code>
attribute(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, Others::[<a href="occi_category.md#type-id">occi_category:id()</a>]) -&gt; <a href="occi_attribute.md#type-t">occi_attribute:t()</a> | undefined
</code></pre>
<br />

throws `{unknown_category, term()}`

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(CatId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; [<a href="occi_attribute.md#type-t">occi_attribute:t()</a>]
</code></pre>
<br />

throws `{unknown_category, [occi_category:id()](occi_category.md#type-id)}`

<a name="categories-0"></a>

### categories/0 ###

<pre><code>
categories() -&gt; [<a href="occi_category.md#type-t">occi_category:t()</a>]
</code></pre>
<br />

Return the list of categories
Categories references (parent, depends, etc) are not resolved

<a name="category-1"></a>

### category/1 ###

<pre><code>
category(Id::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a> | undefined
</code></pre>
<br />

Return a category
Category references (parent, depend, etc) is resolved:
attributes and actions from references are merged into the resulting category

<a name="import-1"></a>

### import/1 ###

<pre><code>
import(E::<a href="occi_extension.md#type-t">occi_extension:t()</a>) -&gt; ok
</code></pre>
<br />

Import an extension into the model

<a name="kind-2"></a>

### kind/2 ###

<pre><code>
kind(Parent::link | resource, KindId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a>
</code></pre>
<br />

throws `{unknown_category, [occi_category:id()](occi_category.md#type-id)} | {invalid_kind, [occi_category:id()](occi_category.md#type-id)}`

Return a kind, checking it has specified parent

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

