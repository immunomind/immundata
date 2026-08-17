

# Get Immundata internal schema field names

## Description

Returns the standardized field names used across Immundata objects and
processing functions, as defined in <code>IMD_GLOBALS$schema</code>.
These include column names for cell ids or barcodes, receptors,
repertoires, and related metadata.

## Usage

<pre><code class='language-R'>imd_schema(key = NULL)

imd_schema_sym(key = NULL)

imd_meta_schema()

imd_files()

imd_rename_cols(format = "default")

imd_drop_cols(format = "airr")

imd_repertoire_schema(format = "airr")

imd_receptor_features(schema)

imd_receptor_chains(schema)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="key">key</code>
</td>
<td>
Character which field to return.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="format">format</code>
</td>
<td>
Character what format to load - "airr" or "10x".
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="schema">schema</code>
</td>
<td>
Receptor schema from <code>make_receptor_schema()</code>.
</td>
</tr>
</table>
