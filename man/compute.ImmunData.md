

# Compute ImmunData annotations

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/operations_compute_collect.R#L16)

## Description

Materializes the annotation table of an <code>ImmunData</code> object
via <code>dplyr::compute()</code> and returns a new
<code>ImmunData</code>.

## Usage

<pre><code class='language-R'>## S3 method for class 'ImmunData'
compute(x, ...)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
ImmunData object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
Additional arguments passed to <code>dplyr::compute()</code> for
<code>x$annotations</code>.
</td>
</tr>
</table>

## Value

A new <code>ImmunData</code> object with computed annotations and the
input repertoire, strata, schema, and provenance state preserved.
