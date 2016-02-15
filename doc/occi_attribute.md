

# Module occi_attribute #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-name_t">name_t()</a> ###


<pre><code>
name_t() = string()
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = #{}
</code></pre>




### <a name="type-type_t">type_t()</a> ###


<pre><code>
type_t() = <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default-1">default/1</a></td><td></td></tr><tr><td valign="top"><a href="#default-2">default/2</a></td><td></td></tr><tr><td valign="top"><a href="#mutable-1">mutable/1</a></td><td></td></tr><tr><td valign="top"><a href="#mutable-2">mutable/2</a></td><td></td></tr><tr><td valign="top"><a href="#name-1">name/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#required-1">required/1</a></td><td></td></tr><tr><td valign="top"><a href="#required-2">required/2</a></td><td></td></tr><tr><td valign="top"><a href="#title-1">title/1</a></td><td></td></tr><tr><td valign="top"><a href="#title-2">title/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default-1"></a>

### default/1 ###

<pre><code>
default(A::<a href="#type-t">t()</a>) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

<a name="default-2"></a>

### default/2 ###

<pre><code>
default(Value::<a href="occi_type.md#type-t">occi_type:t()</a>, A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="mutable-1"></a>

### mutable/1 ###

<pre><code>
mutable(A::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="mutable-2"></a>

### mutable/2 ###

<pre><code>
mutable(Mutable::boolean(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="name-1"></a>

### name/1 ###

<pre><code>
name(A::<a href="#type-t">t()</a>) -&gt; <a href="#type-name_t">name_t()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Name::string(), Type::<a href="#type-type_t">type_t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="required-1"></a>

### required/1 ###

<pre><code>
required(A::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="required-2"></a>

### required/2 ###

<pre><code>
required(Required::boolean(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="title-1"></a>

### title/1 ###

<pre><code>
title(A::<a href="#type-t">t()</a>) -&gt; string()
</code></pre>
<br />

<a name="title-2"></a>

### title/2 ###

<pre><code>
title(Title::string(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(A::<a href="#type-t">t()</a>) -&gt; <a href="#type-type_t">type_t()</a>
</code></pre>
<br />

