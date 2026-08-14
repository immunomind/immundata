

# Collect ImmunData annotations

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_compute_collect.R#L39)

## Description

Collects annotations from an <code>ImmunData</code> object and returns
them as a tibble. Factor columns are converted to character.

## Usage

<pre><code class='language-R'>## S3 method for class 'ImmunData'
collect(x, ...)
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
Additional arguments passed to <code>dplyr::collect()</code> for
<code>x$annotations</code>.
</td>
</tr>
</table>

## Value

A tibble with collected annotations.
