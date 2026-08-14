

# Aggregate repertoires into strata

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_agg_strata.R#L42)

## Description

Creates a strata layer above repertoires by grouping
<code>idata$repertoires</code> with user-selected metadata columns.

This enables a hierarchy of <code>chains -\> barcodes -\> receptors -\>
repertoires -\> strata</code>.

## Usage

<pre><code class='language-R'>agg_strata(idata, schema, strata_name_prefix = "Strata")
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An <code>ImmunData</code> object with repertoire aggregation already
available (run <code>agg_repertoires()</code> first).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
Character vector of columns in <code>idata$repertoires</code> used to
define strata.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="strata_name_prefix">strata_name_prefix</code>
</td>
<td>
Character(1). Prefix for automatic strata labels in
<code>strata_name</code>. Default: <code>“Strata”</code>.
</td>
</tr>
</table>

## Details

Strata are derived from the current repertoire definition. Calling
<code>agg_strata()</code> on an already stratified object replaces the
previous strata mapping, and the generated <code>imd_strata_id</code>
values should be treated as internal identifiers rather than stable
identifiers.

Calling <code>agg_repertoires()</code> after <code>agg_strata()</code>
intentionally rebuilds the repertoire identifiers and statistics.
Because the previous strata mapping is tied to the old repertoire state,
<code>imd_strata_id</code>, <code>strata_name</code>, and the strata
schema are removed rather than carried forward. To retain a strata layer
after redefining or recomputing repertoires, call
<code>agg_strata()</code> again with the desired schema. The same rule
applies when another operation re-aggregates repertoires internally.

## Value

A new <code>ImmunData</code> object where:

<ul>
<li>

<code style="white-space: pre;">$repertoires</code> includes
<code>imd_strata_id</code> and <code>strata_name</code>;

</li>
<li>

<code style="white-space: pre;">$annotations</code> includes
<code>imd_strata_id</code> (joined by <code>imd_repertoire_id</code>).

</li>
</ul>

<code>strata_name</code> is stored only in
<code style="white-space: pre;">$repertoires</code> and is not copied
into <code style="white-space: pre;">$annotations</code>.

## See Also

<code>agg_repertoires()</code>, <code>rename_strata()</code>, ImmunData
