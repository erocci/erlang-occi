

# Module occi_uri #
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
t() = #uri{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append_path-2">append_path/2</a></td><td>Append a path to the existing path of the system.</td></tr><tr><td valign="top"><a href="#from_string-1">from_string/1</a></td><td>Parse uri.</td></tr><tr><td valign="top"><a href="#from_string-2">from_string/2</a></td><td>Parse uri, eventually completing with host/port and path's
prefix from context.</td></tr><tr><td valign="top"><a href="#path_strip-1">path_strip/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>Render uri as binary.</td></tr><tr><td valign="top"><a href="#to_string-2">to_string/2</a></td><td>Render uri as binary, with a different context.</td></tr><tr><td valign="top"><a href="#urn-1">urn/1</a></td><td>Given a (binary) seed, return a urn.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="append_path-2"></a>

### append_path/2 ###

<pre><code>
append_path(Uri::<a href="#type-t">t()</a>, NewPath::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Append a path to the existing path of the system

<a name="from_string-1"></a>

### from_string/1 ###

<pre><code>
from_string(S::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Parse uri

<a name="from_string-2"></a>

### from_string/2 ###

<pre><code>
from_string(Url::binary() | string(), Ctx::<a href="occi_ctx.md#type-t">occi_ctx:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Parse uri, eventually completing with host/port and path's
prefix from context

<a name="path_strip-1"></a>

### path_strip/1 ###

`path_strip(Bin) -> any()`

<a name="to_string-1"></a>

### to_string/1 ###

<pre><code>
to_string(Uri::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Render uri as binary

<a name="to_string-2"></a>

### to_string/2 ###

<pre><code>
to_string(Uri::<a href="uri.md#type-t">uri:t()</a>, Ctx::<a href="occi_ctx.md#type-t">occi_ctx:t()</a>) -&gt; binary()
</code></pre>
<br />

Render uri as binary, with a different context

<a name="urn-1"></a>

### urn/1 ###

<pre><code>
urn(Seed::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Given a (binary) seed, return a urn

