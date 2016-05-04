

# Module occi_ctx #
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
t() = <a href="maps.md#type-map">maps:map()</a>
</code></pre>




### <a name="type-validation">validation()</a> ###


<pre><code>
validation() = server | client | internal | model
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#client-1">client/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(client, Url)</tt></a>.</td></tr><tr><td valign="top"><a href="#model-0">model/0</a></td><td>Equivalent to <a href="#new-2"><tt>new(model, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#model-1">model/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(model, Url)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Check type:
<ul>
<li>server: immutable attributes can be changed, required ones must be set</li>
<li>client: immutable attributes can not be set</li>
<li>internal: for entity sub-types. Do not check if all required attributes are set (for instance in link constructor)</li>
</ul></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a rendering context.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="client-1"></a>

### client/1 ###

`client(Url) -> any()`

Equivalent to [`new(client, Url)`](#new-2).

<a name="model-0"></a>

### model/0 ###

`model() -> any()`

Equivalent to [`new(model, undefined)`](#new-2).

<a name="model-1"></a>

### model/1 ###

`model(Url) -> any()`

Equivalent to [`new(model, Url)`](#new-2).

<a name="new-1"></a>

### new/1 ###

`new(Url) -> any()`

Equivalent to [`new(internal, Url)`](#new-2).

Check type:

* server: immutable attributes can be changed, required ones must be set

* client: immutable attributes can not be set

* internal: for entity sub-types. Do not check if all required attributes are set (for instance in link constructor)


<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Validation::<a href="#type-validation">validation()</a>, Url::<a href="occi_uri.md#type-t">occi_uri:t()</a> | binary() | undefined) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a rendering context

