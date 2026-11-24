

# Annotate an AnnData object from ImmunData (by barcode)

[**Source code**](https://github.com/immunomind/immundata/tree/3dfb9178af910d588d53ae2bff114cd18e49d5ca/R/operations_external_annotate_anndatar.R#L14)

## Description

Copy selected columns from <code>idata</code> to <code>adata$obs</code>.
Match rows by cell barcode (<code>adata$obs_names</code>).

## Usage

<pre><code class='language-R'>annotate_anndata(idata, adata, cols)
</code></pre>

## Arguments

<table data-quarto-postprocess="true" role="presentation">
<tbody>
<tr>
<td
style="white-space: nowrap; font-family: monospace; vertical-align: top"><code
id="idata">idata</code></td>
<td>An ImmunData object.</td>
</tr>
<tr>
<td
style="white-space: nowrap; font-family: monospace; vertical-align: top"><code
id="adata">adata</code></td>
<td>An anndataR::AbstractAnnData object.</td>
</tr>
<tr>
<td
style="white-space: nowrap; font-family: monospace; vertical-align: top"><code
id="cols">cols</code></td>
<td>Character vector with names of columns in <code>idata</code> to
copy.</td>
</tr>
</tbody>
</table>

## Value

The updated AnnData object.
