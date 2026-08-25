

# Add external information to ImmunData

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_annotate.R#L227)

## Description

Use the <code style="white-space: pre;">annotate\_\*()</code> functions
to add information stored in another data frame to an ImmunData object.
For example, you can add cell types from a single-cell analysis, antigen
labels for receptors, or clinical information for samples.

When matching identifiers are unique, the functions keep every row in
<code>idata</code>. When a row has no match in <code>annotations</code>,
the new columns contain <code>NA</code>. Each function returns a new
ImmunData object. The original object is not changed.

## Usage

<pre><code class='language-R'>annotate_immundata(
  idata,
  annotations,
  by,
  keep_repertoires = TRUE,
  remove_limit = FALSE,
  conflicts = c("error", "replace")
)

annotate(
  idata,
  annotations,
  by,
  keep_repertoires = TRUE,
  remove_limit = FALSE,
  conflicts = c("error", "replace")
)

annotate_receptors(
  idata,
  annotations,
  annot_col = imd_schema("receptor"),
  keep_repertoires = TRUE,
  remove_limit = FALSE,
  conflicts = c("error", "replace")
)

annotate_barcodes(
  idata,
  annotations,
  annot_col = "&lt;rownames&gt;",
  keep_repertoires = TRUE,
  remove_limit = FALSE,
  conflicts = c("error", "replace")
)

annotate_chains(
  idata,
  annotations,
  annot_col = imd_schema("chain"),
  keep_repertoires = TRUE,
  remove_limit = FALSE,
  conflicts = c("error", "replace")
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An ImmunData object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="annotations">annotations</code>
</td>
<td>
A data frame containing the information to add. It must contain the
columns used for matching and at most one row for each matching
identifier or combination of identifiers.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="by">by</code>
</td>
<td>
A named character vector describing how columns are matched. Names are
columns in <code>idata</code>; values are the corresponding columns in
<code>annotations</code>. For example, <code>c(Response =
“response_code”)</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="keep_repertoires">keep_repertoires</code>
</td>
<td>
Whether to preserve existing repertoire and strata summaries without
recalculation. The default is <code>TRUE</code>. If <code>FALSE</code>,
these summaries and their derived annotation columns are removed.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="remove_limit">remove_limit</code>
</td>
<td>
Whether to allow an annotation table with 100 or more columns. The
default is <code>FALSE</code>, which stops the operation for such
tables. Set to <code>TRUE</code> only when the wide join is intentional.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="conflicts">conflicts</code>
</td>
<td>
How to handle new annotation columns whose names already exist in
<code>idata</code>. <code>“error”</code>, the default, stops the
operation. <code>“replace”</code> replaces existing columns that are not
protected by <code>ImmunData</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="annot_col">annot_col</code>
</td>
<td>
Name of the identifier column in <code>annotations</code>. For
<code>annotate_receptors()</code> and <code>annotate_chains()</code>,
the default is the standard <code>ImmunData</code> receptor or chain
identifier. For <code>annotate_barcodes()</code>, the default
<code>“\<rownames\>”</code> uses the row names of
<code>annotations</code>. Supplying an explicit barcode column is
usually clearer.
</td>
</tr>
</table>

## Details

The functions differ in how they select the columns used for matching.
The rules for duplicate identifiers, column conflicts, and preserved
summaries are the same for all functions.

## Value

A new ImmunData object containing the added annotation columns. Existing
repertoire and strata summaries are preserved when
<code>keep_repertoires = TRUE</code>.

## Choose a function

Use the function that matches the type of information you want to add:

<ul>
<li>

<code>annotate_barcodes()</code> matches cell or barcode identifiers.

</li>
<li>

<code>annotate_receptors()</code> matches receptor identifiers. All rows
belonging to a matched receptor receive the new information.

</li>
<li>

<code>annotate_chains()</code> matches chain identifiers.

</li>
<li>

<code>annotate()</code> matches any one or more columns that you specify
in <code>by</code>.

</li>
</ul>

The first three functions select the correct <code>ImmunData</code>
identifier for you. <code>annotate_immundata()</code> is an alternative
name for <code>annotate()</code>.

## How matching works

For <code>annotate_barcodes()</code>, <code>annotate_receptors()</code>,
and <code>annotate_chains()</code>, <code>annot_col</code> names the
identifier column in <code>annotations</code>. For example,
<code>annot_col = “barcode”</code> matches the <code>barcode</code>
column in <code>annotations</code> with the barcode identifier in
<code>idata</code>.

For general matching, supply <code>by</code> in the form
<code>c(immundata_column = “annotation_column”)</code>. For example,
<code>by = c(Response = “response_code”)</code> matches the
<code>Response</code> column in <code>idata</code> with the
<code>response_code</code> column in <code>annotations</code>. To match
columns with the same name, use a value such as <code>by = c(Response =
“Response”)</code>. You can include more than one pair of columns in
<code>by</code>.

Columns from <code>annotations</code> that are not used for matching are
added to the result. Rows in <code>idata</code> without a match receive
<code>NA</code>. Rows in <code>annotations</code> without a match are
ignored.

## Annotation identifiers must be unique

<code>annotations</code> must contain at most one row for each
identifier, or each combination of identifiers when matching several
columns. For example, a barcode annotation table must contain at most
one row per barcode.

The function does not check this rule because the annotation table may
be very large. If an identifier occurs several times, the corresponding
rows in <code>idata</code> are repeated. This can make receptor counts,
proportions, and other summaries incorrect.

## Existing annotation columns

By default, the function stops if a new annotation column has the same
name as a column already present in <code>idata</code>. This prevents
accidental replacement.

Use <code>conflicts = “replace”</code> to replace existing annotation
columns. Columns that define receptors, repertoires, strata, or other
<code>ImmunData</code> state are protected and cannot be replaced. The
old column is removed before matching, so rows without a new match
receive <code>NA</code>.

## Repertoire and strata summaries

With the default <code>keep_repertoires = TRUE</code>, existing
repertoire and strata summaries are copied to the new object without
recalculation. Use this option when you are only adding information and
the matching identifiers in <code>annotations</code> are unique.

Set <code>keep_repertoires = FALSE</code> when you plan to filter rows
or define new repertoires using the added information. This removes
existing repertoire and strata summaries and their derived columns.
After annotation and filtering, use <code>agg_repertoires()</code> to
define the new repertoires.

## Very wide annotation tables

By default, the function stops when <code>annotations</code> contains
100 or more columns. Adding a very wide table, such as a complete
gene-expression matrix, can be slow and require a large amount of
memory. If you understand this cost, set <code>remove_limit =
TRUE</code> to allow the operation.

## See Also

<code>dplyr::left_join()</code>, <code>agg_repertoires()</code>,
<code>filter_immundata()</code>, <code>mutate_immundata()</code>,
ImmunData

## Examples

``` r
library("immundata")

library(immundata)
library(dplyr)

options(immundata.verbose = FALSE)

# Load data included with immundata
idata <- get_test_idata()

# Add cell types by matching barcode identifiers
cell_labels <- tibble(
  barcode = c("S1_1", "S1_2"),
  cell_type = c("CD8 T cell", "CD4 T cell")
)

idata_with_cells <- idata |>
  annotate_barcodes(
    annotations = cell_labels,
    annot_col = "barcode"
  )

idata_with_cells |>
  collect() |>
  filter(imd_barcode %in% c("S1_1", "S1_2", "S1_3")) |>
  select(imd_barcode, cell_type) |>
  arrange(imd_barcode)
```

    #> # A tibble: 3 × 2
    #>   imd_barcode cell_type 
    #> * <chr>       <chr>     
    #> 1 S1_1        CD8 T cell
    #> 2 S1_2        CD4 T cell
    #> 3 S1_3        <NA>

``` r
# Expected result:
#   imd_barcode cell_type
#   S1_1        CD8 T cell
#   S1_2        CD4 T cell
#   S1_3        NA

# Add antigen labels to selected receptors
receptor_labels <- tibble(
  receptor_id = c(738L, 1567L),
  antigen = c("CMV", "CMV")
)

idata_with_antigens <- idata |>
  annotate_receptors(
    annotations = receptor_labels,
    annot_col = "receptor_id"
  )

idata_with_antigens |>
  collect() |>
  filter(!is.na(antigen)) |>
  distinct(imd_receptor_id, cdr3_aa, antigen) |>
  arrange(imd_receptor_id)
```

    #> # A tibble: 2 × 3
    #>   imd_receptor_id cdr3_aa       antigen
    #> *           <int> <chr>         <chr>  
    #> 1             738 SVWTSGGNNEQF  CMV    
    #> 2            1567 ASSLEMEGTGELF CMV

``` r
# Expected result:
#   imd_receptor_id cdr3_aa       antigen
#               738 ASRAGAGTGELF  CMV
#              1567 ASFPVLSPYNEQF CMV

# Match columns with different names
response_info <- tibble(
  response_code = c("FR", "PR"),
  response_label = c("Full response", "Partial response")
)

idata_with_response <- idata |>
  annotate(
    annotations = response_info,
    by = c(Response = "response_code")
  )

idata_with_response |>
  collect() |>
  distinct(Response, response_label) |>
  arrange(Response)
```

    #> # A tibble: 2 × 2
    #>   Response response_label  
    #> * <chr>    <chr>           
    #> 1 FR       Full response   
    #> 2 PR       Partial response

``` r
# Expected result:
#   Response response_label
#   FR       Full response
#   PR       Partial response

# Replace an annotation column intentionally
revised_cell_labels <- tibble(
  barcode = c("S1_1", "S1_2"),
  cell_type = c("Cytotoxic T cell", "Helper T cell")
)

idata_with_revised_cells <- idata_with_cells |>
  annotate_barcodes(
    annotations = revised_cell_labels,
    annot_col = "barcode",
    conflicts = "replace"
  )

# Remove old repertoire summaries before defining repertoires by cell type
cell_repertoires <- idata |>
  annotate_barcodes(
    annotations = cell_labels,
    annot_col = "barcode",
    keep_repertoires = FALSE
  ) |>
  filter(!is.na(cell_type)) |>
  agg_repertoires(schema = "cell_type")

cell_repertoires$repertoires |>
  arrange(cell_type)
```

    #> # A tibble: 2 × 4
    #>   imd_repertoire_id cell_type  n_barcodes n_receptors
    #> *             <int> <chr>           <dbl>       <int>
    #> 1                 1 CD4 T cell          1           1
    #> 2                 2 CD8 T cell          1           1

``` r
# Expected result:
#   imd_repertoire_id cell_type  n_barcodes n_receptors
#                   1 CD4 T cell          1           1
#                   2 CD8 T cell          1           1
```
