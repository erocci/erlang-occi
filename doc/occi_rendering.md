

# Module occi_rendering #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Provide common functions for rendering/parsing OCCI types.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load-3">load/3</a></td><td>Load the specified OCCI type from an iolist()
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}.</td></tr><tr><td valign="top"><a href="#load_path-2">load_path/2</a></td><td>Load the specified type from a file.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="load-3"></a>

### load/3 ###

<pre><code>
load(Type::<a href="occi_type.md#type-name">occi_type:name()</a>, MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Bin::iolist()) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

throws `{parse_error, [occi_parser:errors()](occi_parser.md#type-errors)} | {unknown_mimetype, term()}`

Load the specified OCCI type from an iolist()
Mimetype must be given as {Type :: binary(), SubType :: binary(), []}

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}


<a name="load_path-2"></a>

### load_path/2 ###

<pre><code>
load_path(Type::<a href="occi_type.md#type-name">occi_type:name()</a>, Filename::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; ok
</code></pre>
<br />

throws `enoent | eacces | eisdir | enotdir | enomem`

Load the specified type from a file.

Mimetype is detected from file extension.

Supported mimetypes are: xml

<a name="render-2"></a>

### render/2 ###

<pre><code>
render(MimeType::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, T::<a href="occi_type.md#type-t">occi_type:t()</a>) -&gt; iolist()
</code></pre>
<br />

