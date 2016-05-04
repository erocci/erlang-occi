

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




### <a name="type-resource">resource()</a> ###


<pre><code>
resource() = {Class::<a href="occi_type.md#type-name">occi_type:name()</a>, Id::binary(), Kind::<a href="occi_category.md#type-id">occi_category:id()</a>, Mixins::[<a href="occi_category.md#type-id">occi_category:id()</a>], Attributes::<a href="maps.md#type-map">maps:map()</a>, Values::<a href="maps.md#type-map">maps:map()</a>, Actions::<a href="maps.md#type-map">maps:map()</a>, Links::list()}
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = <a href="#type-resource">resource()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_link-2">add_link/2</a></td><td>Add the given link to the resource.</td></tr><tr><td valign="top"><a href="#from_map-2">from_map/2</a></td><td>New resource from AST.</td></tr><tr><td valign="top"><a href="#links-1">links/1</a></td><td>Get list of links associated to this resource.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a resource with given id, of kind ...core#resource.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_link-2"></a>

### add_link/2 ###

<pre><code>
add_link(Link::<a href="occi_link.md#type-t">occi_link:t()</a>, R::<a href="occi_resource.md#type-t">occi_resource:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Add the given link to the resource

<a name="from_map-2"></a>

### from_map/2 ###

<pre><code>
from_map(Kind::<a href="occi_kind.md#type-t">occi_kind:t()</a>, Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

New resource from AST

<a name="links-1"></a>

### links/1 ###

<pre><code>
links(R::<a href="occi_resource.md#type-t">occi_resource:t()</a>) -&gt; [<a href="occi_link.md#type-t">occi_link:t()</a>]
</code></pre>
<br />

Get list of links associated to this resource

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Id::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a resource with given id, of kind ...core#resource

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::binary(), KindId::<a href="occi_category.md#type-t">occi_category:t()</a> | <a href="occi_category.md#type-id">occi_category:id()</a> | string() | binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{unknown_category, term()}`

