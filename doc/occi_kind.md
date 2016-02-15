

# Module occi_kind #
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
t() = #{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#parent-1">parent/1</a></td><td></td></tr><tr><td valign="top"><a href="#parent-2">parent/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Scheme::string(), Term::string()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="parent-1"></a>

### parent/1 ###

<pre><code>
parent(Kind::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

<a name="parent-2"></a>

### parent/2 ###

<pre><code>
parent(Parent::string() | binary() | <a href="occi_category.md#type-id">occi_category:id()</a>, Kind::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

