

# Modify or Add Columns to ImmunData Annotations

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_mutate.R#L157)

## Description

Applies transformations to the
<code style="white-space: pre;">$annotations</code> table within an
<code>ImmunData</code> object, similar to <code>dplyr::mutate</code>. It
allows adding new columns or modifying existing non-schema columns using
standard <code>dplyr</code> expressions. Additionally, it can add new
columns based on sequence comparisons (exact match, regular expression
matching, or distance calculation) against specified patterns.

## Usage

<pre><code class='language-R'>mutate_immundata(idata, ..., seq_options = NULL)

# S3 method for class 'ImmunData'
mutate(.data, ..., seq_options = NULL)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>, <code id=".data">.data</code>
</td>
<td>
An <code>ImmunData</code> object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
<code>dplyr::mutate</code>-style named expressions (e.g., <code>new_col
= existing_col \* 2</code>, <code>category = ifelse(value \> 10, “high”,
“low”)</code>). These are applied first. <strong>Important</strong>: You
cannot use names for new or modified columns that conflict with internal
<code>ImmunData</code> columns, receptor features, or
repertoire-defining columns.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seq_options">seq_options</code>
</td>
<td>
Optional named list specifying sequence-based annotation options. Use
<code>make_seq_options()</code> for convenient creation. See
<code>filter_immundata</code> documentation
(<code>?filter_immundata</code>) or the details section here for the
list structure (<code>query_col</code>, <code>patterns</code>,
<code>method</code>, <code>name_type</code>). <code>max_dist</code> is
ignored for mutation. If <code>NULL</code> (the default), no
sequence-based columns are added.
</td>
</tr>
</table>

## Details

The function operates in two main steps:

<ol>
<li>

<strong>Standard Mutations (<code>…</code>)</strong>: Applies the
standard <code>dplyr::mutate</code>-style expressions provided in
<code>…</code> to the
<code style="white-space: pre;">$annotations</code> table. You can
create new columns or modify existing ones, but you <em>cannot</em>
modify internal system columns, receptor features, or
repertoire-defining columns. An error will occur if you attempt to do
so.

</li>
<li>

<strong>Sequence-based Annotations (<code>seq_options</code>)</strong>:
If <code>seq_options</code> is provided, the function calculates
sequence similarities or distances and adds corresponding new columns to
the <code style="white-space: pre;">$annotations</code> table.

<ul>
<li>

<code>method = “exact”</code>: Adds boolean columns (TRUE/FALSE)
indicating whether the <code>query_col</code> value exactly matches each
<code>pattern</code>. Column names are generated using a prefix (e.g.,
<code>sim_exact\_</code>) and the pattern or its index.

</li>
<li>

<code>method = “regex”</code>: Uses <code>annotate_tbl_regex</code> to
add columns indicating matches for each regular expression pattern
against the <code>query_col</code>. The exact nature of the added
columns depends on <code>annotate_tbl_regex</code> (e.g., boolean flags
or captured groups).

</li>
<li>

<code>method = “lev”</code> or <code>method = “hamm”</code>: Uses
<code>annotate_tbl_distance</code> to calculate Levenshtein or Hamming
distances between the <code>query_col</code> and each
<code>pattern</code>, adding columns containing these numeric distances.
<code>max_dist</code> is ignored in this context (internally treated as
<code>NA</code>) as all distances are calculated and added, not used for
filtering.

</li>
<li>

The naming of the new sequence-based columns depends on the
<code>name_type</code> option within <code>seq_options</code> and
internal helper functions like <code>make_pattern_columns</code>.
Prefixes like <code>sim_exact\_</code>, <code>sim_regex\_</code>,
<code>dist_lev\_</code>, <code>dist_hamm\_</code> are typically used
based on the schema.

</li>
</ul>
</li>
</ol>

The <code style="white-space: pre;">$repertoires</code> and
<code style="white-space: pre;">$strata</code> tables, if present in the
input <code>idata</code>, are copied to the output object without
modification. This function only affects the
<code style="white-space: pre;">$annotations</code> table.

## Value

A <em>new</em> <code>ImmunData</code> object with the
<code style="white-space: pre;">$annotations</code> table modified
according to the provided expressions and <code>seq_options</code>. The
<code style="white-space: pre;">$repertoires</code> and
<code style="white-space: pre;">$strata</code> tables (if present) are
carried over unchanged from the input <code>idata</code>.

## See Also

<code>dplyr::mutate()</code>, <code>make_seq_options()</code>,
<code>filter_immundata()</code>, ImmunData,
<code>vignette(“immundata-classes”, package = “immunarch”)</code>
(replace with actual package name if different)

## Examples

``` r
library("immundata")

# Basic setup (assuming idata_test is a valid ImmunData object)
# print(idata_test)

# Example 1: Add a simple derived column
idata_mut1 <- mutate(idata_test, V_family = substr(V_gene, 1, 5))
print(idata_mut1$annotations)

# Example 2: Add multiple columns and modify one (if 'custom_score' exists)
# Note: Avoid modifying core schema columns like 'V_gene' itself.
idata_mut2 <- mutate(idata_test,
  V_basic = gsub("-.*", "", V_gene),
  J_len = nchar(J_gene),
  custom_score = custom_score * 1.1
) # Fails if custom_score doesn't exist
print(idata_mut2$annotations)

# Example 3: Add boolean columns for exact CDR3 matches
cdr3_patterns <- c("CARGLGLVFYGMDVW", "CARDNRGAVAGVFGEAFYW")
seq_opts_exact <- make_seq_options(
  query_col = "CDR3_aa",
  patterns = cdr3_patterns,
  method = "exact",
  name_type = "pattern"
) # Name cols by pattern
idata_mut_exact <- mutate(idata_test, seq_options = seq_opts_exact)
# Look for new columns like 'sim_exact_CARGLGLVFYGMDVW'
print(idata_mut_exact$annotations)

# Example 4: Add Levenshtein distance columns for a CDR3 pattern
seq_opts_lev <- make_seq_options(
  query_col = "CDR3_aa",
  patterns = "CARGLGLVFYGMDVW",
  method = "lev",
  name_type = "index"
) # Name col like 'dist_lev_1'
idata_mut_lev <- mutate(idata_test, seq_options = seq_opts_lev)
# Look for new column 'dist_lev_1' (or similar based on schema)
print(idata_mut_lev$annotations)

# Example 5: Combine standard mutation and sequence annotation
seq_opts_regex <- make_seq_options(
  query_col = "V_gene",
  patterns = c(ighv1 = "^IGHV1-", ighv3 = "^IGHV3-"),
  method = "regex",
  name_type = "pattern"
)
idata_mut_combo <- mutate(idata_test,
  chain_upper = toupper(chain),
  seq_options = seq_opts_regex
)
# Look for 'chain_upper' and regex match columns (e.g., 'sim_regex_ighv1')
print(idata_mut_combo)
```
