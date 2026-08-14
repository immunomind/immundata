

# Rename strata labels

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_agg_strata.R#L146)

## Description

Renames <code>strata_name</code> values for existing strata
(<code>imd_strata_id</code>) in <code>idata$repertoires</code>.

This function updates only repertoire-level metadata; annotations remain
unchanged and keep only <code>imd_strata_id</code>.

## Usage

<pre><code class='language-R'>rename_strata(
  idata,
  names,
  unnamed = c("error", "auto", "keep"),
  auto_prefix = "Strata"
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An <code>ImmunData</code> object with strata already created by
<code>agg_strata()</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="names">names</code>
</td>
<td>

Mapping from <code>imd_strata_id</code> to <code>strata_name</code>.
Supported forms:

<ul>
<li>

named character vector, where names are strata IDs;

</li>
<li>

data frame with columns <code>imd_strata_id</code> and
<code>strata_name</code>.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="unnamed">unnamed</code>
</td>
<td>
What to do with strata IDs not covered by <code>names</code>:
<code>“error”</code> (default), <code>“auto”</code>, or
<code>“keep”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="auto_prefix">auto_prefix</code>
</td>
<td>
Prefix used when <code>unnamed = “auto”</code> (or when legacy objects
miss <code>strata_name</code>). Default: <code>“Strata”</code>.
</td>
</tr>
</table>

## Value

A new <code>ImmunData</code> object with updated
<code>strata_name</code> in
<code style="white-space: pre;">$repertoires</code>.

## See Also

<code>agg_strata()</code>, <code>agg_repertoires()</code>
