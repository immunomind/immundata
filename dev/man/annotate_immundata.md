

# Annotate ImmunData object

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/operations_annotate.R#L108)

## Description

Joins additional annotation data to the annotations slot of an
<code>ImmunData</code> object.

This function allows you to add extra information to your repertoire
data by joining a dataframe of annotations based on specified columns.
It supports joining by one or more columns.

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
An <code>ImmunData</code> R6 object containing repertoire and annotation
data.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="annotations">annotations</code>
</td>
<td>
A data frame containing the annotations to be joined.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="by">by</code>
</td>
<td>
A named character vector specifying the columns to join by. The names of
the vector should be the column names in <code>idata$annotations</code>
and the values should be the corresponding column names in the
<code>annotations</code> data frame.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="keep_repertoires">keep_repertoires</code>
</td>
<td>
Logical. If <code>TRUE</code> (default), the existing repertoire and
strata tables, schemas, identifiers, and derived metrics are preserved
without re-aggregation. Set to <code>FALSE</code> to return an
annotations-only object; repertoire- and strata-derived columns are then
removed from the annotation table as well.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="remove_limit">remove_limit</code>
</td>
<td>
Logical. If <code>FALSE</code> (default), a warning will be issued if
the <code>annotations</code> data frame has 100 or more columns,
suggesting potential performance issues. Set to <code>TRUE</code> to
disable this warning and allow joining of annotations with an arbitrary
number of columns. Use with caution, as joining wide dataframes can be
memory-intensive and slow.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="conflicts">conflicts</code>
</td>
<td>
Character scalar controlling annotation value columns that already exist
in <code>idata$annotations</code>. <code>“error”</code> (default)
rejects the collision. <code>“replace”</code> drops the existing columns
before joining the new annotations. Columns defining receptor,
repertoire, or strata state cannot be replaced.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="annot_col">annot_col</code>
</td>
<td>
A character vector specifying the column with receptor, barcode or chain
identifiers to annotate a corresponding receptors, barode or chains in
<code>idata</code>.
</td>
</tr>
</table>

## Details

The function performs a left join operation, keeping all rows from
<code>idata$annotations</code> and adding matching columns from the
<code>annotations</code> data frame.

Annotation join keys are subject to a many-to-one contract: each
combination of the right-hand key columns specified by the values of
<code>by</code> must occur at most once in <code>annotations</code>.
This contract is not checked at runtime because validating a very large
annotation source would require an additional full aggregation.
Supplying non-unique right-hand keys violates the contract and may
expand annotation rows, invalidating the preserved repertoire counts and
proportions.

With <code>keep_repertoires = TRUE</code>, annotation is treated as a
metadata-only transformation. The existing repertoire and strata state
is carried forward unchanged and <code>agg_repertoires()</code> is not
called.

The function uses <code>checkmate</code> to validate the input types and
structure.

A check is performed to ensure that the columns specified in
<code>by</code> exist in both <code>idata$annotations</code> and the
<code>annotations</code> data frame.

The <code>annotations</code> data frame is converted to a duckdb tibble
internally for efficient joining, especially with large datasets.

## Value

A new <code>ImmunData</code> object with the annotations joined to the
<code>annotations</code> slot.

## Warning

By default (<code>remove_limit = FALSE</code>), joining an
<code>annotations</code> data frame with 100 or more columns will
trigger a warning. This is a safeguard to prevent accidental joining of
very wide data (e.g., gene expression data) that could lead to
performance degradation or crashes. If you understand the risks and
intend to join a wide data frame, set <code>remove_limit = TRUE</code>.

## Examples

``` r
library("immundata")

# Assuming 'my_immun_data' is an ImmunData object and 'sample_info' is a data frame
# with a column 'sample_id' matching 'sample' in my_immun_data$annotations
# and additional columns like 'treatment' and 'disease_status'.

sample_info <- data.frame(
  sample_id = c("sample1", "sample2", "sample3", "sample4"),
  treatment = c("Treatment A", "Treatment B", "Treatment A", "Treatment C"),
  disease_status = c("Healthy", "Disease", "Healthy", "Disease"),
  stringsAsFactors = FALSE # Important to keep characters as characters
)

# Join sample information using the 'sample' column
my_immun_data_annotated <- annotate(
  idata = my_immun_data,
  annotations = sample_info,
  by = c("sample" = "sample_id")
)

# New sample_info

# Join data by multiple columns, e.g., 'sample' and 'barcode'
# Assuming 'cell_annotations' is a data frame with 'sample_barcode' and 'cell_type'
my_immun_data_cell_annotated <- annotate(
  idata = my_immun_data,
  annotations = cell_annotations,
  by = c("sample" = "sample", "barcode" = "sample_barcode")
)

# Join a wide dataframe, suppressing the column limit warning
# Assuming 'gene_expression' is a data frame with 'barcode' and many gene columns
my_immun_data_gene_expression <- annotate(
  idata = my_immun_data,
  annotations = gene_expression,
  by = c("barcode" = "barcode"),
  remove_limit = TRUE
)
```
