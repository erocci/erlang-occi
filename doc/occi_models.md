

# Module occi_models #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_category-2">add_category/2</a></td><td></td></tr><tr><td valign="top"><a href="#attribute-2">attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#category-1">category/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>Called on module load.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_category-2"></a>

### add_category/2 ###

<pre><code>
add_category(Scheme::<a href="occi_extension.md#type-id">occi_extension:id()</a>, Cat::<a href="occi_category.md#type-t">occi_category:t()</a>) -&gt; ok
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
attributes(CatId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; #{}
</code></pre>
<br />

throws `{unknown_category, [occi_category:id()](occi_category.md#type-id)}`

<a name="category-1"></a>

### category/1 ###

<pre><code>
category(Id::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a> | undefined
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

Called on module load. Allows unit testing.

