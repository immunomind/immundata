

# Keep selected rows or receptors in ImmunData

## Description

Use <code>filter()</code> to keep selected rows in an ImmunData object.
For example, you can keep rows from one response group, rows using a
selected V gene, or receptors containing a CDR3 sequence similar to a
reference sequence.

The function returns a new ImmunData object. The original object is not
changed.

This function is a direct implementation of dplyr::filter. Alternative
function name is <code>filter_immundata</code>.

Use <code>filter_barcodes()</code> to keep selected cell barcodes and
<code>filter_receptors()</code> to keep selected receptor identifiers.

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
An ImmunData object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
One or more conditions used to keep rows. Refer to annotation columns
directly by name. Multiple conditions are combined with <code>&</code>.
Conditions are applied before sequence matching.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seq_options">seq_options</code>
</td>
<td>
Options for matching sequences with reference sequences or patterns.
Create these options with <code>make_seq_options()</code>. If
<code>NULL</code>, the default, no sequence matching is performed.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="keep_repertoires">keep_repertoires</code>
</td>
<td>
If <code>TRUE</code>, the default, existing repertoire and strata
summaries are recalculated from the filtered data. If
<code>FALSE</code>, the returned object does not contain these
summaries.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id=".by">.by</code>, <code id=".preserve">.preserve</code>
</td>
<td>
Accepted for compatibility with <code>dplyr::filter()</code>, but
currently not used for ImmunData objects.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="barcodes">barcodes</code>
</td>
<td>
A character, integer, or numeric vector of cell barcodes to keep with
<code>filter_barcodes()</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="receptors">receptors</code>
</td>
<td>
A character, integer, or numeric vector of receptor identifiers to keep
with <code>filter_receptors()</code>.
</td>
</tr>
</table>

## Details

You can filter an ImmunData object in three ways:

<ul>
<li>

Supply conditions in <code>…</code> to filter using annotation columns.
Refer to columns directly by name. For example, <code>Response ==
“FR”</code> keeps rows from the <code>FR</code> response group.

</li>
<li>

Supply <code>seq_options</code>, created with
<code>make_seq_options()</code>, to find receptors containing a sequence
that matches one or more reference sequences or patterns.

</li>
<li>

Use <code>filter_barcodes()</code> or <code>filter_receptors()</code>
when you already have the identifiers that you want to keep.

</li>
</ul>

Conditions in <code>…</code> are applied before sequence matching.
Sequence matching then identifies receptors from the remaining rows.
When one chain matches, all remaining chains belonging to the same
receptor are kept. A chain removed by a condition in <code>…</code> is
not added back by sequence matching.

Sequence matching methods are:

<ul>
<li>

<code>“exact”</code>: the sequence must be identical to one of the
references.

</li>
<li>

<code>“regex”</code>: the sequence must match a regular-expression
pattern. This is an advanced option for matching text patterns.

</li>
<li>

<code>“lev”</code>: the Levenshtein distance counts the substitutions,
insertions, or deletions needed to change one sequence into the other.

</li>
<li>

<code>“hamm”</code>: the Hamming distance counts different positions
between sequences of the same length. Sequences of different lengths do
not match.

</li>
</ul>

For <code>“lev”</code> and <code>“hamm”</code>, provide
<code>max_dist</code>. A sequence is accepted when its distance from at
least one reference is less than or equal to this value. A distance of
<code>0</code> means an exact match, and smaller values mean more
similar sequences.

By default, existing repertoire summaries are recalculated from the
filtered data. Existing strata are also rebuilt, and their labels are
retained. Set <code>keep_repertoires = FALSE</code> to return an object
without repertoire or strata summaries.

## Value

A new ImmunData object containing the selected rows and receptors. If
requested, repertoire and strata summaries are recalculated for the
selected data.

## See Also

<code>dplyr::filter()</code>, <code>make_seq_options()</code>,
<code>mutate_immundata()</code>, <code>agg_repertoires()</code>,
ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Load data included with immundata
idata <- get_test_idata()

# Keep rows from one response group
fr_response <- idata |>
  filter(Response == "FR")

fr_response |>
  collect() |>
  summarise(
    n_rows = n(),
    n_receptors = n_distinct(imd_receptor_id)
  )
```

    #> # A tibble: 1 × 2
    #>   n_rows n_receptors
    #> *  <int>       <dbl>
    #> 1    955         871

``` r
# Expected result:
#   n_rows n_receptors
#      955         871

# Keep receptors containing one reference CDR3 sequence
reference_cdr3 <- "ASFPVLSPYNEQF"

exact_match <- idata |>
  filter(
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = reference_cdr3,
      method = "exact"
    )
  )

exact_match |>
  collect() |>
  select(cdr3_aa, v_call, Response)
```

    #> # A tibble: 1 × 3
    #>   cdr3_aa       v_call    Response
    #> * <chr>         <chr>     <chr>   
    #> 1 ASFPVLSPYNEQF TRBV28*01 FR

``` r
# Expected result:
#   cdr3_aa       v_call    Response
#   ASFPVLSPYNEQF TRBV28*01 FR

# Keep receptors within four sequence changes of the reference
similar_sequences <- idata |>
  filter(
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = reference_cdr3,
      method = "lev",
      max_dist = 4
    )
  )

similar_sequences |>
  collect() |>
  distinct(cdr3_aa) |>
  arrange(cdr3_aa)
```

    #> # A tibble: 4 × 1
    #>   cdr3_aa      
    #> * <chr>        
    #> 1 ASFPVLSPYNEQF
    #> 2 ASSPDSPSYNEQF
    #> 3 ASSPGLAAYNEQF
    #> 4 ASSPTLYNEQF

``` r
# Expected result:
#   cdr3_aa
#   ASFPVLSPYNEQF
#   ASSPDSPSYNEQF
#   ASSPGLAAYNEQF
#   ASSPTLYNEQF

# Keep two selected cell barcodes
selected_barcodes <- c("S1_1", "S1_2")

selected_cells <- idata |>
  filter_barcodes(selected_barcodes)

selected_cells |>
  collect() |>
  distinct(imd_barcode)
```

    #> # A tibble: 2 × 1
    #>   imd_barcode
    #> * <chr>      
    #> 1 S1_1       
    #> 2 S1_2

``` r
# Expected result:
#   imd_barcode
#   S1_1
#   S1_2

# The same approach can keep selected receptor identifiers
selected_receptors <- idata |>
  collect() |>
  distinct(imd_receptor_id) |>
  slice_head(n = 2) |>
  pull(imd_receptor_id)

selected_receptors_data <- idata |>
  filter_receptors(selected_receptors)
```
