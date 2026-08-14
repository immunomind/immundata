

# Build a <code>seq_options</code> list for sequence‑based receptor filtering

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_utils.R#L25)

## Description

A convenience wrapper that validates the common arguments for
<strong><code>filter_receptors()</code></strong> and returns them in the
required list form.

## Usage

<pre><code class='language-R'>make_seq_options(
  query_col,
  patterns,
  method = c("exact", "lev", "hamm", "regex"),
  max_dist = NA,
  name_type = c("index", "pattern")
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="query_col">query_col</code>
</td>
<td>
Character(1). Name of the receptor column to compare
(e.g. <code>“cdr3_aa”</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="patterns">patterns</code>
</td>
<td>
Character vector of sequences or regular expressions to search for.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="method">method</code>
</td>
<td>
One of <code>“exact”</code>, <code>“regex”</code>, <code>“lev”</code>
(Levenshtein), or <code>“hamm”</code> (Hamming). Defaults to
<code>“exact”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="max_dist">max_dist</code>
</td>
<td>
Numeric distance threshold for <code>“lev”</code> / <code>“hamm”</code>
filtering. Use <code>NA</code> (default) to keep all rows after
annotation.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="name_type">name_type</code>
</td>
<td>
Passed straight to <code>annotate_tbl_distance()</code>; either
<code>“index”</code> (default) or <code>“pattern”</code>.
</td>
</tr>
</table>

## Value

A named list suitable for the <code>seq_options</code> argument of
<code>filter_receptors()</code>.

## See Also

<code>filter_receptors()</code>, <code>annotate_receptors()</code>
