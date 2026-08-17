

# Annotate an AnnData object from ImmunData (by barcode)

## Description

Copy selected columns from <code>idata$annotations</code> to
<code>adata$obs</code>, matching by cell barcode
(<code>adata$obs_names</code>).

## Usage

<pre><code class='language-R'>annotate_anndata(idata, adata, cols)
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
<code id="adata">adata</code>
</td>
<td>
An anndataR::AbstractAnnData object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="cols">cols</code>
</td>
<td>
Character vector with column names to transfer from
<code>idata$annotations</code>.
</td>
</tr>
</table>

## Value

The updated AnnData object.
