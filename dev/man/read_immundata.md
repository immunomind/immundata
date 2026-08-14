

# Load a saved ImmunData from disk

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_immundata_read.R#L74)

## Description

Reconstructs an <code>ImmunData</code> object from files previously
saved to a directory by <code>write_immundata()</code> or the internal
saving step of <code>read_repertoires()</code>. It reads the
<code>annotations.parquet</code> file for the main data and
<code>metadata.json</code> to retrieve the necessary receptor and
repertoire schemas.

## Usage

<pre><code class='language-R'>read_immundata(
  path,
  tag = NULL,
  version = NULL,
  prudence = "stingy",
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="path">path</code>
</td>
<td>
Character(1). Path to the <strong>directory</strong> containing the
saved <code>ImmunData</code> files (<code>annotations.parquet</code> and
<code>metadata.json</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tag">tag</code>
</td>
<td>
Character(1) or <code>NULL</code>. Optional snapshot tag to load from
<code style="white-space: pre;">path/snapshots/\<tag\>/vNNN</code>. When
provided, <code>path</code> must point to the project/home folder.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="version">version</code>
</td>
<td>
Integer(1) or <code>NULL</code>. Optional snapshot version number to
load within a tag (e.g. <code>1</code> means <code>v001</code>). If
<code>NULL</code>, the latest version for the tag is loaded.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="prudence">prudence</code>
</td>
<td>
Character(1). Controls strictness of type inference when reading the
Parquet file, passed to <code>duckplyr::read_parquet_duckdb()</code>.
Default <code>“stingy”</code> likely implies stricter type checking or
safer inference.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Logical(1). Whether to print informative messages. Defaults to
<code>getOption(“immundata.verbose”, TRUE)</code>.
</td>
</tr>
</table>

## Details

This function expects a directory structure created by
<code>write_immundata()</code>, containing at least:

<ul>
<li>

<code>annotations.parquet</code>: The main annotation data table.

</li>
<li>

<code>metadata.json</code>: Contains package version,
receptor/repertoire/strata schemas, the repertoire table, current
<code>snapshot_id</code>, lineage events, and provenance paths.

</li>
</ul>

The loading process involves:

<ol>
<li>

Checking that the specified <code>path</code> is a directory and
contains the required <code>annotations.parquet</code> and
<code>metadata.json</code> files.

</li>
<li>

Reading <code>metadata.json</code> using
<code>jsonlite::read_json()</code>.

</li>
<li>

Reading <code>annotations.parquet</code> using
<code>duckplyr::read_parquet_duckdb()</code> with the specified
<code>prudence</code> level.

</li>
<li>

Restoring the receptor, repertoire, and strata schemas and the
serialized repertoire table from metadata.

</li>
<li>

Instantiating a new <code>ImmunData</code> object directly, without
re-aggregating repertoires or strata.

</li>
</ol>

## Value

A new <code>ImmunData</code> object reconstructed from the saved files.

## See Also

<code>write_immundata()</code> for saving <code>ImmunData</code>
objects, <code>read_repertoires()</code> for the primary data loading
pipeline, ImmunData class, <code>agg_repertoires()</code> for repertoire
definition.

## Examples

``` r
library("immundata")

# Assume 'my_idata' is an ImmunData object created previously
# my_idata <- read_repertoires(...)

# Define a temporary directory for saving
save_dir <- tempfile("saved_immundata_")

# Save the ImmunData object
write_immundata(my_idata, save_dir)

# --- Later, in a new session or script ---

# Load the ImmunData object back from the directory
loaded_idata <- read_immundata(save_dir)

# Verify the loaded object
print(loaded_idata)
# compare_methods(my_idata$annotations, loaded_idata$annotations) # If available

# Clean up
unlink(save_dir, recursive = TRUE)
```
