

# Filter ImmunData by receptor features, barcodes or any annotations

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_filter.R#L146)

## Description

Provides flexible filtering options for an <code>ImmunData</code>
object.

<code>filter()</code> is the main function, allowing filtering based on
receptor features (e.g., CDR3 sequence) using various matching methods
(exact, regex, fuzzy) and/or standard <code>dplyr</code>-style filtering
on annotation columns.

<code>filter_barcodes()</code> is a convenience function to filter by
specific cell barcodes.

<code>filter_receptors()</code> is a convenience function to filter by
specific receptor identifiers.

## Usage

<pre><code class='language-R'>filter_immundata(idata, ..., seq_options = NULL, keep_repertoires = TRUE)

# S3 method for class 'ImmunData'
filter(
  .data,
  ...,
  .by = NULL,
  .preserve = FALSE,
  seq_options = NULL,
  keep_repertoires = TRUE
)

filter_barcodes(idata, barcodes, keep_repertoires = TRUE)

filter_receptors(idata, receptors, keep_repertoires = TRUE)
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
For <code>filter</code>, these are regular <code>dplyr</code>-style
filtering expressions (e.g., <code>V_gene == “IGHV1-1”</code>,
<code>chain == “IGH”</code>) applied to the
<code style="white-space: pre;">$annotations</code> table
<em>before</em> sequence filtering. Ignored by
<code>filter_barcodes</code> and <code>filter_receptors</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seq_options">seq_options</code>
</td>
<td>

For <code>filter</code>, an optional named list specifying
sequence-based filtering options. Use <code>make_seq_options()</code>
for convenient creation. The list can contain:

<ul>
<li>

<code>query_col</code> (Character scalar): The name of the column in
<code style="white-space: pre;">$annotations</code> containing sequences
to compare (e.g., <code>“CDR3_aa”</code>, <code>“FR1_nt”</code>).

</li>
<li>

<code>patterns</code> (Character vector): A vector of sequences or
regular expressions to match against <code>query_col</code>.

</li>
<li>

<code>method</code> (Character scalar): The matching method. One of
<code>“exact”</code>, <code>“regex”</code>, <code>“lev”</code>
(Levenshtein distance), or <code>“hamm”</code> (Hamming distance).
Defaults typically handled by <code>make_seq_options</code>.

</li>
<li>

<code>max_dist</code> (Numeric scalar): For fuzzy methods
(<code>“lev”</code>, <code>“hamm”</code>), the maximum allowed distance.
Rows with distance \<= <code>max_dist</code> are kept. Defaults
typically handled by <code>make_seq_options</code>.

</li>
<li>

<code>name_type</code> (Character scalar): Determines column names in
intermediate distance calculations if applicable (<code>“index”</code>
or <code>“pattern”</code>). Passed through to internal annotation
functions. Defaults typically handled by <code>make_seq_options</code>.
If <code>seq_options</code> is <code>NULL</code> (the default), no
sequence-based filtering is performed.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="keep_repertoires">keep_repertoires</code>
</td>
<td>
Logical scalar. If <code>TRUE</code> (the default) and the input
<code>idata</code> has repertoire information
(<code>idata$schema_repertoire</code> is not <code>NULL</code>), the
repertoire summaries will be recalculated based on the filtered data
using <code>agg_repertoires()</code>. If the input has strata, they are
rebuilt from the existing strata schema and existing labels are
retained. If <code>FALSE</code>, or if no repertoire schema exists, the
returned <code>ImmunData</code> object will not contain repertoire
summaries (<code style="white-space: pre;">$repertoires</code> will be
<code>NULL</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id=".by">.by</code>
</td>
<td>
Not used.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id=".preserve">.preserve</code>
</td>
<td>
Not used.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="barcodes">barcodes</code>
</td>
<td>
For <code>filter_barcodes</code>, a vector of cell identifiers
(barcodes) to keep. Can be character, integer, or numeric.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="receptors">receptors</code>
</td>
<td>
For <code>filter_receptors</code>, a vector of receptor identifiers to
keep. Can be character, integer, or numeric.
</td>
</tr>
</table>

## Details

For <code>filter</code>:

<ul>
<li>

User-provided <code>dplyr</code>-style filters (<code>…</code>) are
applied <em>before</em> any sequence-based filtering defined in
<code>seq_options</code>.

</li>
<li>

Sequence filtering compares values in the <code>query_col</code> of the
annotations table against the provided <code>patterns</code>.

</li>
<li>

Supported sequence matching methods are:

<ul>
<li>

<code>“exact”</code>: Keeps rows where <code>query_col</code> exactly
matches any of the <code>patterns</code>.

</li>
<li>

<code>“regex”</code>: Keeps rows where <code>query_col</code> matches
any of the regular expressions in <code>patterns</code>.

</li>
<li>

<code>“lev”</code> (Levenshtein distance): Keeps rows where the edit
distance between <code>query_col</code> and any pattern is less than or
equal to <code>max_dist</code>.

</li>
<li>

<code>“hamm”</code> (Hamming distance): Keeps rows where the Hamming
distance (for equal length strings) between <code>query_col</code> and
any pattern is less than or equal to <code>max_dist</code>.

</li>
</ul>
</li>
<li>

The filtering operations act on the
<code style="white-space: pre;">$annotations</code> table. A new
<code>ImmunData</code> object is created containing only the rows (and
corresponding receptors) that pass the filter(s).

</li>
<li>

If <code>keep_repertoires = TRUE</code> (and repertoire data exists in
the input), the repertoire-level summaries
(<code style="white-space: pre;">$repertoires</code> table) are
recalculated based on the filtered annotations. Otherwise, the
<code style="white-space: pre;">$repertoires</code> table in the output
will be <code>NULL</code>.

</li>
</ul>

For <code>filter_barcodes</code> and <code>filter_receptors</code>:

<ul>
<li>

These functions provide a simpler interface for common filtering tasks
based on cell barcodes or receptor IDs, respectively. They use efficient
<code>semi_join</code> operations internally.

</li>
</ul>

## Value

A new <code>ImmunData</code> object containing only the filtered
annotations (and potentially recalculated repertoire summaries). The
schema remains the same.

## See Also

<code>make_seq_options()</code>, <code>dplyr::filter()</code>,
<code>agg_repertoires()</code>, ImmunData

## Examples

``` r
library("immundata")

# Basic setup (assuming idata_test is a valid ImmunData object)
# print(idata_test)

# --- filter examples ---
# Example 1: dplyr-style filtering on annotations
filtered_heavy <- filter(idata_test, chain == "IGH")
print(filtered_heavy)

# Example 2: Exact sequence matching on CDR3 amino acid sequence
cdr3_patterns <- c("CARGLGLVFYGMDVW", "CARDNRGAVAGVFGEAFYW")
seq_opts_exact <- make_seq_options(query_col = "CDR3_aa", patterns = cdr3_patterns)
filtered_exact_cdr3 <- filter(idata_test, seq_options = seq_opts_exact)
print(filtered_exact_cdr3)

# Example 3: Combining dplyr-style and fuzzy sequence matching (Levenshtein)
seq_opts_lev <- make_seq_options(
  query_col = "CDR3_aa",
  patterns = "CARGLGLVFYGMDVW",
  method = "lev",
  max_dist = 1
)
filtered_combined <- filter(idata_test,
  chain == "IGH",
  C_gene == "IGHG1",
  seq_options = seq_opts_lev
)
print(filtered_combined)

# Example 4: Regex matching on V gene
v_gene_pattern <- "^IGHV[13]-" # Keep only IGHV1 or IGHV3 families
seq_opts_regex <- make_seq_options(
  query_col = "V_gene",
  patterns = v_gene_pattern,
  method = "regex"
)
filtered_regex_v <- filter(idata_test, seq_options = seq_opts_regex)
print(filtered_regex_v)

# Example 5: Filtering without recalculating repertoires
filtered_no_rep <- filter(idata_test, chain == "IGK", keep_repertoires = FALSE)
print(filtered_no_rep) # $repertoires should be NULL


# --- filter_barcodes example ---
# Assuming 'cell1_barcode' and 'cell5_barcode' exist in idata_test$annotations$cell_id
specific_barcodes <- c("cell1_barcode", "cell5_barcode")
filtered_cells <- filter_barcodes(idata_test, barcodes = specific_barcodes)
print(filtered_cells)


# --- filter_receptors example ---
# Assuming receptor IDs 101 and 205 exist in idata_test$annotations$receptor_id
specific_receptors <- c(101, 205) # Or character IDs if applicable
filtered_recs <- filter_receptors(idata_test, receptors = specific_receptors)
print(filtered_recs)
```
