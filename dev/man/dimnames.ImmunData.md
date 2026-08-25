

# Get Annotation Dimnames from ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_dimnames.R#L13)

## Description

Returns dimension names for an <code>ImmunData</code> object so that
<code>colnames(idata)</code> maps to annotation column names.

## Usage

<pre><code class='language-R'>## S3 method for class 'ImmunData'
dimnames(x)
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
</table>

## Value

A list with <code>NULL</code> row names and annotation column names.
