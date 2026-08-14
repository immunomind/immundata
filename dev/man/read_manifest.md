

# Load and Validate a Manifest for Immune Repertoire Files

[**Source code**](https://github.com/immunomind/immundata/tree/dev/R/io_manifest_read.R#L39)

## Description

This function loads a manifest from either a file path or a data frame,
validates the presence of a column with repertoire file paths, and
converts all file paths to absolute paths. It is used to support
flexible pipelines for loading bulk or single-cell immune repertoire
data across samples.

If the input is a file path, the function reads it with
<code>readr::read_delim</code>. If the input is a data frame, it checks
whether file paths are absolute; relative paths are only allowed when
the manifest is loaded from a file.

It warns the user if many of the files listed in the manifest are
missing, and stops execution if none of the files exist.

The column with file paths is normalized into the internal filename
schema.

## Usage

<pre><code class='language-R'>read_manifest(
  manifest,
  file_col = "file",
  delim = NULL,
  ...,
  verbose = getOption("immundata.verbose", TRUE)
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="manifest">manifest</code>
</td>
<td>

A manifest table. Can be either:

<ul>
<li>

a data frame with per-file annotations,

</li>
<li>

or a path to a CSV/TSV/TXT manifest file.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="file_col">file_col</code>
</td>
<td>
A string specifying the name of the column in the manifest that contains
paths to repertoire files. Defaults to <code>“file”</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="delim">delim</code>
</td>
<td>
Delimiter used to read the manifest file. If <code>NULL</code>, it is
inferred from the extension: comma for <code>.csv</code>, tab for
<code>.tsv</code> and <code>.txt</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
Additional arguments passed to <code>readr::read_delim()</code> when
reading a manifest from a file.
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

## Value

A validated and updated manifest data frame with absolute file paths and
an additional internal column named <code>imd_filename</code>.
