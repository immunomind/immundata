

# Downsample ImmunData annotations at the repertoire level

## Description

Downsamples an <code>ImmunData</code> object by selecting barcodes
within each repertoire. If repertoire schema is not defined, the entire
dataset is treated as one repertoire.

The function uses a single parameter <code>n</code>:

<ul>
<li>

If <code style="white-space: pre;">0 \< n \< 1</code>, <code>n</code> is
treated as a proportion of repertoire size.

</li>
<li>

If <code>n \>= 1</code>, <code>n</code> is treated as an absolute target
count. In particular, <code>n = 1</code> retains one barcode/receptor
unit per repertoire for cell-level data, or one count per repertoire for
bulk data.

</li>
</ul>

Downsampling is barcode-based. For count-based inputs (e.g. bulk),
per-barcode counts (<code>imd_n_chains</code>) are trimmed when needed
to reach the target. Existing repertoire-derived metrics
(<code>imd_count</code>, <code>imd_proportion</code>,
<code>n_repertoires</code>, <code>n_receptors</code>,
<code>n_barcodes</code>) are dropped and recalculated for the
downsampled object when a repertoire schema is present. Original metrics
are not retained because they describe the pre-downsampled universe. If
the input has a strata schema, the strata layer is rebuilt from that
schema after repertoire aggregation. Existing strata labels are
retained.

## Usage

<pre><code class='language-R'>downsample_immundata(idata, n, seed = NULL)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
An <code>ImmunData</code> object.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="n">n</code>
</td>
<td>
Numeric scalar controlling downsampling amount. See details for
interpretation.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seed">seed</code>
</td>
<td>
Optional integer scalar for reproducible sampling.
</td>
</tr>
</table>

## Value

A new <code>ImmunData</code> object with downsampled annotations.
