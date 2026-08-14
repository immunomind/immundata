

# Save ImmunData to disk

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_immundata_write.R#L99)

## Description

Serializes the essential components of an <code>ImmunData</code> object
to disk for efficient storage and later retrieval. It saves the core
annotation data (<code>idata$annotations</code>) as a compressed Parquet
file and accompanying metadata (including schemas, repertoire data, and
package version) as a JSON file within a specified directory.

## Usage

<pre><code class='language-R'>write_immundata(
  idata,
  output_folder = NULL,
  tag = NULL,
  rehome = FALSE,
  compression = "zstd",
  compression_level = 9,
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="idata">idata</code>
</td>
<td>
The <code>ImmunData</code> object to save. Must be an R6 object of class
<code>ImmunData</code> containing at least the
<code style="white-space: pre;">$annotations</code> table and schema
information (<code style="white-space: pre;">$schema_receptor</code>,
optionally <code style="white-space: pre;">$schema_repertoire</code> and
<code style="white-space: pre;">$schema_strata</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="output_folder">output_folder</code>
</td>
<td>
Character(1) or <code>NULL</code>. Path to the directory where the
output files will be written. If <code>NULL</code>, a snapshot directory
is created as
<code style="white-space: pre;">home_path/snapshots/\<tag\>/vNNN</code>,
where <code>home_path</code> is read from internal
<code>ImmunData</code> provenance.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tag">tag</code>
</td>
<td>
Character(1) or <code>NULL</code>. Snapshot tag used only when
<code>output_folder = NULL</code> (for example,
<code>“baseline”</code>). If <code>NULL</code>, defaults to
<code>“default”</code> for auto-snapshots.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="rehome">rehome</code>
</td>
<td>
Logical(1). If <code>TRUE</code>, and <code>output_folder</code> is
explicitly provided, this folder becomes the new snapshot home for
future auto-snapshots. Default: <code>FALSE</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="compression">compression</code>
</td>
<td>
Character(1) or <code>NULL</code>. Parquet compression codec passed
through to DuckDB (via <code>duckplyr::compute_parquet(options =
…)</code>). Defaults to <code>“zstd”</code>. Set <code>NULL</code> to
let DuckDB choose.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="compression_level">compression_level</code>
</td>
<td>
Numeric(1) or <code>NULL</code>. Compression level passed through to
DuckDB for codecs that support levels (for example, Zstandard). Defaults
to <code>9</code>. Set <code>NULL</code> to let DuckDB choose.
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

The function performs the following actions:

<ol>
<li>

Validates the input <code>idata</code> object and write options.

</li>
<li>

Resolves the destination folder:

<ul>
<li>

uses <code>output_folder</code> when explicitly provided, or

</li>
<li>

creates an auto-snapshot folder under
<code style="white-space: pre;">home_path/snapshots/\<tag\>/vNNN</code>
when <code>output_folder = NULL</code>.

</li>
</ul>
</li>
<li>

Constructs metadata including schemas, the repertoire table,
<code>snapshot_id</code>, lineage, and provenance paths.

</li>
<li>

Writes metadata to <code>metadata.json</code> within the resolved output
folder.

</li>
<li>

Writes the <code>idata$annotations</code> table (a
<code>duckplyr_df</code> or similar) to <code>annotations.parquet</code>
within <code>output_folder</code>.

<ul>
<li>

By default, uses <code>compression = “zstd”</code> and
<code>compression_level = 9</code>.

</li>
<li>

A common choice is <code>compression = “snappy”</code> for faster
reads/writes with larger files.

</li>
<li>

Another common choice is <code>compression = “zstd”</code> for smaller
files, often with higher CPU cost.

</li>
<li>

<code>compression_level</code> usually trades speed for size (higher
levels: smaller output but slower processing).

</li>
<li>

Compatibility note: for <code>duckplyr</code> version
<code style="white-space: pre;">1.2.0</code>,
<code>compute_parquet()</code> does not accept extra options due to a
known issue. In that version, compression-related arguments are ignored
and DuckDB defaults are used.

</li>
</ul>
</li>
<li>

Uses internal helper <code>imd_files()</code> to determine the standard
filenames (<code>metadata.json</code>,
<code>annotations.parquet</code>).

</li>
</ol>

The receptor data itself is not stored separately; receptors remain a
view derived from annotations and the receptor schema.

## Value

Invisibly returns the input <code>idata</code> object, saved to disk. In
other words, this allows you to create snapshots of the data in the
<code>output_folder</code>. Mind that by saving the object, you execute
all the stored computations, so this operations can take longer than
expected. Read more about snapshots on our website in the
<a href="https://immunomind.github.io/docs/concepts/basics/immutability/">"Concept"
section</a>.

## See Also

<code>read_immundata()</code> for loading the saved data,
<code>read_repertoires()</code> which uses this function internally,
ImmunData class definition.

## Examples

``` r
library("immundata")

# Assume 'my_idata' is an ImmunData object created previously
# my_idata <- read_repertoires(...)

# Define an output directory
save_dir <- tempfile("saved_immundata_")

# Save the ImmunData object
write_immundata(my_idata, save_dir)

# Auto-snapshot under <home>/snapshots/baseline/vNNN
write_immundata(my_idata, tag = "baseline")

# Optional: request a specific parquet compression setup
write_immundata(my_idata, save_dir, compression = "zstd", compression_level = 9)

# Optional: let DuckDB choose both settings
write_immundata(my_idata, save_dir, compression = NULL, compression_level = NULL)

# Check the created files
list.files(save_dir) # Should show "annotations.parquet" and "metadata.json"

# Clean up
unlink(save_dir, recursive = TRUE)
```
