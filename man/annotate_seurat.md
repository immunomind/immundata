

# Annotate a Seurat object from ImmunData (by barcode)

[**Source code**](https://github.com/immunomind/immundata/tree/main/R/operations_external_annotate_seurat.R#L38)

## Description

Copy selected columns from <code>idata$annotations</code> to Seurat
metadata using the cell barcode. This is the simplest way to transfer
data from <code>immundata</code> to Seurat object, e.g., for plotting
data on UMAP.

## Usage

<pre><code class='language-R'>annotate_seurat(idata, sdata, cols)
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
<code id="sdata">sdata</code>
</td>
<td>
A Seurat object (cells are columns; barcodes are
<code>colnames(sdata)</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="cols">cols</code>
</td>
<td>
Character vector with column names to transfer from
<code>idata$annotations</code>. Typical choices:
<code>“clonal_prop_bin”</code> or <code>“clonal_rank_bin”</code>.
</td>
</tr>
</table>

## Details

See functions <code>annotate_clonality_rank</code> and
<code>annotate_clonality_prop</code> in <code>immunarch</code> package.

## Value

The updated Seurat object with new metadata columns.

## See Also

ImmunData, SeuratObject::AddMetaData

## Examples

``` r
library("immundata")

# After annotating receptors:
idata <- annotate_clonality_prop(idata)

# Transfer the clonality bin to Seurat and plot:
sdata <- annotate_seurat(idata, sdata, cols = "clonal_prop_bin")
Seurat::DimPlot(sdata, reduction = "umap", group.by = "clonal_prop_bin", shuffle = TRUE)

# Alternative: rank bins
idata <- annotate_clonality_rank(idata, bins = c(10, 100))
sdata <- annotate_seurat(idata, sdata, cols = "clonal_rank_bin")
```
