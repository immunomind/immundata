

# Add or change annotation columns in ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/operations_mutate.R#L247)

## Description

Use <code>mutate()</code> to add information to each row of an ImmunData
object. For example, you can calculate CDR3 length, mark sequences of
interest, or compare receptor sequences with reference sequences.

The function returns a new ImmunData object. The original object is not
changed.

This function is a direct implementation of dplyr::mutate. Alternative
function name is <code>mutate_immundata</code>.

## Usage

<pre><code class='language-R'>mutate_immundata(idata, ..., .by = NULL, seq_options = NULL)

# S3 method for class 'ImmunData'
mutate(.data, ..., .by = NULL, seq_options = NULL)
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
One or more named calculations in the form <code>new_column =
calculation</code>. Refer to existing columns directly by name. You can
add new annotation columns or change columns that are not protected.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id=".by">.by</code>
</td>
<td>
Optional columns used to form temporary groups for this operation. For
example, <code>.by = Response</code> calculates separately for each
response, and <code>.by = c(Response, imd_group_id)</code> uses each
response and receptor-cluster combination. The grouping applies only to
this <code>mutate()</code> call.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seq_options">seq_options</code>
</td>
<td>
Options for comparing sequences with reference sequences or patterns.
Create these options with <code>make_seq_options()</code>. If
<code>NULL</code>, the default, no sequence comparisons are performed.
</td>
</tr>
</table>

## Details

You can use <code>mutate()</code> in three ways:

<ul>
<li>

Supply named calculations in <code>…</code> to create annotation columns
from existing data. For example, <code>cmv_specific = cdr3_aa %in%
cmv_cdr3s</code> adds a column containing <code>TRUE</code> or
<code>FALSE</code> for each row.

</li>
<li>

Supply <code>.by</code> to perform calculations separately for temporary
groups. The number of rows does not change. A group statistic is
repeated for all rows in that group.

</li>
<li>

Supply <code>seq_options</code>, created with
<code>make_seq_options()</code>, to compare a sequence column with one
or more reference sequences or patterns. One result column is added for
each reference.

</li>
</ul>

Named calculations in <code>…</code> are performed before sequence
comparisons.

Most grouped calculations are translated directly to DuckDB. Some group
statistics, such as <code>n_distinct()</code>, are not available as
DuckDB window calculations when a large dataset must stay on disk. In
that case, <code>mutate()</code> automatically calculates one summary
row per group and joins the values back to the annotation rows. This
remains lazy and does not load the full dataset into R memory.

The automatic fallback works when every calculation in the call produces
one value per group. If a call combines a row-level calculation with a
group statistic that needs the fallback, use two <code>mutate()</code>
calls. Also use a second call when a later calculation refers to a group
statistic created by the fallback. See the examples below.

Columns used to identify receptors or repertoires, and identifiers
managed by <code>ImmunData</code>, are protected. This prevents
accidental changes that would make the object inconsistent. You can add
new columns and change other annotation columns.

Sequence comparison methods are:

<ul>
<li>

<code>“exact”</code>: <code>TRUE</code> when the sequence is identical
to the reference.

</li>
<li>

<code>“regex”</code>: <code>TRUE</code> when the sequence matches a
regular-expression pattern. This is an advanced option for matching text
patterns.

</li>
<li>

<code>“lev”</code>: the number of substitutions, insertions, or
deletions needed to change one sequence into the other.

</li>
<li>

<code>“hamm”</code>: the number of different positions between sequences
of the same length. Sequences with different lengths receive
<code>NA</code>.

</li>
</ul>

For the distance methods, <code>0</code> means an exact match and
smaller values mean more similar sequences. With <code>name_type =
“index”</code>, the result columns have short names such as
<code>imd_sim_exact_1</code> or <code>imd_sim_lev_1</code>. With
<code>name_type = “pattern”</code>, each column name includes its
reference pattern.

<code>max_dist</code> is used by <code>filter_immundata()</code> but has
no effect here because <code>mutate()</code> reports every calculated
distance.

Existing repertoire and strata summaries are carried to the new object
without modification.

## Value

A new ImmunData object containing the added or changed annotation
columns. Existing repertoire and strata summaries are preserved.

## See Also

<code>dplyr::mutate()</code>, <code>make_seq_options()</code>,
<code>filter_immundata()</code>, <code>annotate_receptors()</code>,
<code>agg_repertoires()</code>, ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Load data included with immundata
idata <- get_test_idata()

# Add the length of each CDR3 amino acid sequence
idata_with_length <- idata |>
  mutate(cdr3_length = dd$length(cdr3_aa))

idata_with_length |>
  collect() |>
  select(cdr3_aa, cdr3_length) |>
  slice_head(n = 3)
```

    #> # A tibble: 3 × 2
    #>   cdr3_aa       cdr3_length
    #> * <chr>               <dbl>
    #> 1 ASFPVLSPYNEQF          13
    #> 2 ASRAGAGTGELF           12
    #> 3 ASSPGQGLDTQY           12

``` r
# Expected result:
#   cdr3_aa       cdr3_length
#   ASFPVLSPYNEQF          13
#   ASRAGAGTGELF           12
#   ASSPGQGLDTQY           12

# Compare CDR3 sequences with one reference sequence
reference_cdr3 <- "ASFPVLSPYNEQF"

idata_with_matches <- idata |>
  mutate(
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = reference_cdr3,
      method = "exact"
    )
  )

idata_with_matches |>
  collect() |>
  count(imd_sim_exact_1)
```

    #> # A tibble: 2 × 2
    #>   imd_sim_exact_1     n
    #> * <lgl>           <int>
    #> 1 FALSE            1901
    #> 2 TRUE                1

``` r
# Expected result:
#   imd_sim_exact_1     n
#   FALSE            1901
#   TRUE                1

# Calculate Levenshtein distance from the reference sequence
idata_with_distance <- idata |>
  mutate(
    seq_options = make_seq_options(
      query_col = "cdr3_aa",
      patterns = reference_cdr3,
      method = "lev"
    )
  )

idata_with_distance |>
  collect() |>
  select(cdr3_aa, imd_sim_lev_1) |>
  arrange(imd_sim_lev_1, cdr3_aa) |>
  slice_head(n = 3)
```

    #> # A tibble: 3 × 2
    #>   cdr3_aa       imd_sim_lev_1
    #> * <chr>                 <dbl>
    #> 1 ASFPVLSPYNEQF             0
    #> 2 ASSPDSPSYNEQF             4
    #> 3 ASSPGLAAYNEQF             4

``` r
# Expected result:
#   cdr3_aa       imd_sim_lev_1
#   ASFPVLSPYNEQF             0
#   ASSPDSPSYNEQF             4
#   ASSPGLAAYNEQF             4

# Mark selected sequences
cmv_cdr3s <- c(
  "ASFPVLSPYNEQF",
  "ASRAGAGTGELF"
)

marked_sequences <- idata |>
  mutate(
    cmv_specific = cdr3_aa %in% cmv_cdr3s
  )

marked_sequences |>
  collect() |>
  count(cmv_specific)
```

    #> # A tibble: 2 × 2
    #>   cmv_specific     n
    #> * <lgl>        <int>
    #> 1 FALSE         1900
    #> 2 TRUE             2

``` r
# Expected result:
#   cmv_specific     n
#   FALSE         1900
#   TRUE             2

# Mark selected receptor identities
cmv_hits <- tibble(
  imd_receptor_id = c(1L, 105L),
  cmv_specific = TRUE
)

marked_receptors <- idata |>
  annotate_receptors(cmv_hits) |>
  mutate(
    cmv_specific = coalesce(cmv_specific, FALSE)
  )

marked_receptors |>
  collect() |>
  count(cmv_specific)
```

    #> # A tibble: 2 × 2
    #>   cmv_specific     n
    #> * <lgl>        <int>
    #> 1 FALSE         1899
    #> 2 TRUE             3

``` r
# Expected result:
#   cmv_specific     n
#   FALSE         1898
#   TRUE             4

# Add response-level statistics to every annotation row
# `.by` means: calculate separately for each response.
response_stats <- idata |>
  mutate(
    response_n_rows = n(),
    response_n_receptors = n_distinct(imd_receptor_id),
    .by = Response
  )

response_stats |>
  collect() |>
  distinct(Response, response_n_rows, response_n_receptors) |>
  arrange(Response)
```

    #> # A tibble: 2 × 3
    #>   Response response_n_rows response_n_receptors
    #> * <chr>              <int>                <dbl>
    #> 1 FR                   955                  871
    #> 2 PR                   947                  867

``` r
# Expected result:
#   Response response_n_rows response_n_receptors
#   FR                   955                  871
#   PR                   947                  867

# A grouped calculation can also produce a different value for every row.
response_centered <- idata |>
  mutate(
    centered_counts = counts - mean(counts, na.rm = TRUE),
    .by = Response
  )

# Do not combine that row-level calculation with a statistic that needs the
# automatic summary fallback in the same call:
# idata |>
#   mutate(
#     centered_counts = counts - mean(counts, na.rm = TRUE),
#     response_n_receptors = n_distinct(imd_receptor_id),
#     .by = Response
#   )

# Use two mutate calls instead. The work remains lazy in DuckDB.
response_details <- idata |>
  mutate(
    centered_counts = counts - mean(counts, na.rm = TRUE),
    .by = Response
  ) |>
  mutate(
    response_n_receptors = n_distinct(imd_receptor_id),
    .by = Response
  )

# Also use a second call when a new calculation uses a statistic created by
# the fallback.
response_details <- idata |>
  mutate(
    response_n_receptors = n_distinct(imd_receptor_id),
    .by = Response
  ) |>
  mutate(
    twice_response_n_receptors = response_n_receptors * 2
  )
```
