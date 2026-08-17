

# Create options for comparing receptor sequences

## Description

Create sequence comparison options for the <code>seq_options</code>
argument of <code>filter_immundata()</code> or
<code>mutate_immundata()</code>. Use these options to compare a sequence
column with one or more reference sequences or patterns.

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
Name of the sequence column to compare, such as <code>“cdr3_aa”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="patterns">patterns</code>
</td>
<td>
One or more reference sequences or regular-expression patterns.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="method">method</code>
</td>
<td>
Comparison method: <code>“exact”</code>, <code>“regex”</code>,
<code>“lev”</code> (Levenshtein distance), or <code>“hamm”</code>
(Hamming distance). The default is <code>“exact”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="max_dist">max_dist</code>
</td>
<td>
Maximum distance accepted by <code>filter_immundata()</code> when
<code>method = “lev”</code> or <code>method = “hamm”</code>. A value is
required when filtering with either distance method. This argument has
no effect on <code>mutate_immundata()</code>, which reports every
calculated distance.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="name_type">name_type</code>
</td>
<td>
How result columns created by <code>mutate_immundata()</code> are named.
<code>“index”</code>, the default, creates short numbered names.
<code>“pattern”</code> includes the reference pattern in each name. This
argument does not change which receptors are kept by
<code>filter_immundata()</code>.
</td>
</tr>
</table>

## Value

A named list for the <code>seq_options</code> argument of
<code>filter_immundata()</code> or <code>mutate_immundata()</code>.

## See Also

<code>filter_immundata()</code>, <code>mutate_immundata()</code>,
<code>annotate_receptors()</code>
