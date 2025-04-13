---
editor_options: 
  markdown: 
    wrap: 72
output: 
  html_document: 
    toc: true
---

# `immundata` in R

## Installation

### Prerequisites

Before installing any release or pre-release version of `immundata`,
please install `pak` that will simplify the installation of any package,
not just `immundata`:

``` r
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
```

More info if needed is available on [R pak
website](https://pak.r-lib.org/#arrow_down-installation).

### Latest CRAN release

``` r
pak::pkg_install("immundata")
```

### Latest GitHub release

Because releasing on CRAN is limited to one release per one or two
months or if the above command doesn't work, try installing the very
latest release of `immundata` from the code repository:

``` r
pak::pkg_install("immunomind/immundata-rlang")
```

### Latest development version

If you are willing to try unstable yet bleeding edge features, or if
there are some hot fix for your open GitHub ticket, please install the
development version via:

``` r
pak::pkg_install("immunomind/immundata-rlang@dev")
```

## Quick start

``` r
library(immundata)

md_path <- system.file("extdata", "metadata_samples.tsv", package = "immundata")
samples <- c(
  system.file("extdata", "sample_0_1k.tsv", package = "immundata"), 
  system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )

md <- read_metadata(md_path)
imdata <- read_repertoires(samples, c("cdr3_aa", "v_call"), md, output_folder = "./immundata-quick-start")
```

## Input / Output

### Read one or multiple AIRR files into `immundata`

**`immundata`** provides a flexible system for loading immune receptor
repertoire files from different sources -- CSV, TSV and Parquet files,
possibly gzipped, with some optionality. The main function for this is
`read_repertoires()`. Below are four ways to pass your file paths.

#### 1. Pass a single file name

If you just have **one** AIRR file:

``` r
library(immundata)

# In this example, we assume 'my_airr_file.tsv' has columns like 'V_gene', 'J_gene', 'CDR3_nt'
# that you want to aggregate into a receptor signature.
my_immdata <- read_repertoires(
  path   = "my_airr_file.tsv",
  schema = c("V_gene", "J_gene", "CDR3_nt")
)

# The output is an ImmunData object, which you can inspect:
my_immdata$receptors()
my_immdata$annotations()
```

-   The `schema` argument tells `immundata` which columns define the
    unique receptor signature.
-   By default, `read_repertoires()` writes Parquet files into a
    directory named `immundata-my_airr_file` and then calls
    `read_immundata()` on it. Consider passing `output_folder` if you
    want to specify the output path.

#### 2. Pass a vector of file names

For **multiple** files in a vector:

``` r
many_files <- c("sample1.airr.tsv", "sample2.airr.tsv", "sample3.airr.tsv")

my_immdata <- read_repertoires(
  path   = many_files,
  schema = c("V_gene", "J_gene", "CDR3_nt")
)
```

`immundata` automatically merges them (depending on your chosen schema)
and writes the aggregated data into a single directory of Parquet files.

#### 3. Pass a glob pattern

If your files follow a consistent naming pattern, you can leverage shell
globs:

``` r
# For example, all AIRR files in the 'samples/' folder
my_immdata <- read_repertoires(
  path   = "samples/*.airr.tsv",
  schema = c("V_gene", "J_gene", "CDR3_nt")
)
```

Behind the scenes, `read_repertoires()` expands the glob with
`Sys.glob(...)`, merges the data, and produces a single `ImmunData`.
Think about it as a huge table instead of smaller multiple repertoire
tables.

#### 4. Use a metadata file

Sometimes you need more control over the data source (e.g. consistent
sample naming, extra columns). In that case:

1.  **Load metadata** with `read_metadata()`.\
2.  **Pass** the resulting data frame to
    `read_repertoires(path = "<metadata>", ..., metadata = md)`. Mind
    the `"<metadata>"` string we pass to the function. It indicates that
    we should take file paths from the input metadata table.

For example:

``` r
# Suppose metadata.tsv has a column 'File' with paths to your AIRR files
md <- read_metadata("metadata.tsv", filename_col = "File")
my_immdata <- read_repertoires(
  path     = "<metadata>"
  metadata = md,
  schema   = c("V_gene", "J_gene", "CDR3_nt")
)
```

This approach **unifies** sample-level metadata (e.g. donor ID,
timepoint) with your repertoire data inside a single `ImmunData`.

### Alternative to reading from files: convert data lists from `immunarch`

`from_immunarch`

### Working with the repertoire metadata file

By design, **`immundata`** splits the data-loading pipeline into
**three** steps, rather than one giant function. This promotes
modularity, easier debugging, and flexible usage:

1.  **(Optional) Load the metadata** via `read_metadata()`.
    -   This ensures your metadata has the correct file paths, absolute
        or relative.\
2.  **Load the repertoire files** from disk via `read_repertoires()`.
    -   This function unifies your data (be it 1 file or 100 files) and
        **outputs** two Parquet files:
        -   **`receptors.parquet`** (receptor-level aggregation)\
        -   **`annotations.parquet`** (cell-level data, sample metadata,
            etc.)\
    -   It then calls `read_immundata()` to return a fully instantiated
        `ImmunData` object that uses the newly created files on the disk
        as a source. The helps two-fold: you don't lose your data, and
        it allows `immundata` to run an optimized code when necessary.\
3.  **(Optionally) Load the same `ImmunData` files later** with
    [`read_immundata()`](#).
    -   If you need to reopen the data in a future R session, you don’t
        have to redo the entire pipeline.\
    -   Just call `read_immundata(path_to_immundata_folder)` where the
        folder contains `receptors.parquet` and `annotations.parquet`.

#### Why split it up?

-   **Modularity**: If something breaks, you can debug whether it’s in
    metadata parsing or the actual repertoire table creation.\
-   **Reusability**: If you have a standard set of metadata files for
    different experiments, you can re-run or share them without
    re-writing everything.
-   **Performance**: Once your data is in `immundata` format, you can
    load it in future sessions in **constant time** without merging or
    parsing again.

#### Example workflow

``` r
# 1) Read the metadata to ensure correct file paths
md <- read_metadata("my_experiment_metadata.tsv", filename_col = "File")

# 2) Read the actual repertoire data, merging the metadata
#    and specifying which columns define the receptor
my_immdata <- read_repertoires(
  path     = md[[IMD_GLOBALS$schema$filename]],
  metadata = md,
  schema   = c("V_gene", "J_gene", "CDR3_nt"),
  output_folder = "immundata-run1"
)

# 3) If you reopen R, just call read_immundata() next time
other_session_data <- read_immundata("immundata-run1")
```

With this approach, you **never** need to re-parse your raw AIRR files
once you’ve generated the Parquet-based `immundata` format.

### Re-aggregating data using receptor and repertoire schemas

This is the key concept that distinguished `immundata` from
DataFrame-based libraries.

-   people analyse a specific receptors
-   data lineage is crucial for full reproducibility

### Modalities of the data source

#### Bulk -- RepSeq, AIRRSeq

TODO

#### Single-cell -- scRNAseq, scVDJseq, scTCRseq, scBCRseq

-   load annotation data
-   do something
-   write the annotation data back
-   visualize AIRR with annotations data
-   visualize SC with annotation data

#### Paired-chain -- scVDJseq or other technologies

TODO

#### Spatial -- spatial transcriptomics and cell coordinates

-   load annotation data
-   do something
-   write the annotation data back
-   visualize AIRR with annotations data
-   visualize SC with annotation data

#### Annotate immune receptors using external AIRR databases

TODO

#### Immunogenicity -- run external tools such as TCRdist to annotate ImmunData

TODO

#### Hybrid datasets

##### Multi-locus data

TODO

##### Multiple contigs for TCR

TODO

##### BCR-heavy chains with multiple light chains

TODO

##### Bulk and single-cell data integration

TODO

### Preprocessing strategies

-   filtering non productive
-   double contigs
-   double BCR chains
-   locus

## Data manipulation

### Filtering

#### Filter by receptor features or their identifiers

`filter_receptors`

one, several, regex, edit distance

#### Filter by annotation

`filter_annotations`

one, several values, several columns

#### Filter by cells identifiers or barcodes

`filter_cells`

list of barcodes

#### Filter by repertoire

`filter_repertoires`

TODO

### Annotations

#### Annotate by receptor feature

`annotate_receptors`

#### Annotate by receptor ID

`annotate_receptors`

#### Annotate by barcode a.k.a. cell ID

`annotate_cells`

### Analyse the data

#### Basic analysis in `immundata`

Find receptors from several repertoires or groups which have \>2
abundance

#### Exporatory and statistical analysis in `immunarch`

TODO

## Advanced topics

### Integrate into your package

TODO

### Change RAM limits to accelerate the backend computations

TODO

### Save your intermediate data for faster computations and reproducibility

TODO

## About

### Citation

TODO

### License

TODO

### Author and contributors

TODO

### Commercial usage

`immundata` is free to use for commercial usage. However, corporate
users will not get a prioritized support for `immundata`-related issues,
immune repertoire analysis questions or data engineering questions,
related to building scalable immune repertoire and other -omics
pipelines. The priority of open-source tool `immundata` is open-source
science.

If you are looking for prioritized support and setting up your data
pipelines, consider contacting Vadim Nazarov for commercial consulting
and support options.

## FAQ

1.  **Q: Why all the function names or ImmunData fields are so long? I
    want to write `imdata$rec` instead of `imdata$receptors`.**

    A: Two major reasons - improving the code readability and motivation
    to leverage the autocomplete tools.

    Please consider using `tab` for leveraging autocomplete. It
    accelerates things x10-20.

2.  **Q: How does `immundata` works under the hood, in simpler terms?**

    A: `immundata` uses the fantastic `duckplyr` package

    References:

    -   [duckplyr](https://duckplyr.tidyverse.org/index.html)

3.  **Q: Why do you need to create Parquet files with receptors and
    annotations?**

    A: First of all, you can turn it off. Second, those are intermediate
    files, optimized for future data operations, and working with them
    significantly accelerates `immundata`. Take a look at our benchmark
    page to learn more: `link`

4.  **Q: Why does `immundata` support only the AIRR standard?!**

    A: The practical answer is that `immundata` allows some level of
    optionality - you can provide column names for barcodes, etc.\
    \
    The short answer is that we have our own standards already.\
    \
    The long answer is that the amount of investments required not only
    for the devlopment, but also for continued support of parsers for
    different formats is astounishing. I delopved parsers for 10 formats
    for tcR / immunarch packages, and I would much prefer for upstream
    tool developers to not change their format each minor versions,
    breaking pretty much all downstream pipelines and causing all sorts
    of pain to end users and tools developers - mind you, without
    bearing a responsibility to at least notify, but ideally fix the
    broken formats they introduced. The time of Wild West is over - the
    AIRR community did an outstanding job creating it's standard. Please
    urge the creators of your favorite tools or your fellow developers
    to use this format or a superset of it, like immundata does.\
    \
    Immundata is not and will not support other formats explicitly. This
    is both a practical stance and communication of crucial values, put
    into immundata as a part of a broader ecosystem of AIRR tools. The
    domains is already too complex, and we need to work together to make
    this complexity manageable.

5.  **Q: Why is it so complex? Why do we need to use `dplyr` instead of
    plain R?**

    A: The short answer is:

    -   faster computations,
    -   code, that is easy to maintain and support by other humans,
    -   and better data skills,
    -   in most cases you don't really need a complexity, so we can
        optimize the 95% of all AIRR data operations

    For the long answer, let me give you more details on each of the
    bullepoint.

6.  **Q: How do I get to use all operations from `dplyr`? `duckplyr`
    doesn't support some operations, which I need.**

    A: Let's consider several use cases.

    **Case 0.** You are missing `group_by` from `dplyr`.

    **Case 1.** Your data can fit into RAM.

    **Case 2.** Your data won't fit into RAM, and you really need to
    work on all of this data.

    **Case 3.** Your data won't fit into RAM, but before running
    intensive computations, you are open to working with smaller dataset
    first.

7.  **Q: You filter out non-productive receptors. How do I explore
    them?**

    A: option for saving non-productive chains to a separate file

8.  **Q: Why does `immundata` have its own column names for receptors
    and repertoires? Could you just use the AIRR format - repertoire_id
    etc.?**

    A: The power of `immundata` lies in the fast re-aggregation of the
    data, that allows to work with whatever you define as a repertoire
    on the fly via `ImmunData$build_repertoires(schema = ...)`

9.  **Q: What do I do with following error: "Error in
    `compute_parquet()` at [...]: !
    {"exception_type":"IO","exception_message":"Failed to write [...]:
    Failed to read file [...]: schema mismatch in glob: column [...] was
    read from the original file [...], but could not be found in file
    [...] If you are trying to read files with different schemas, try
    setting union_by_name=True"}?**\*

    A: It means that your repertoire files have different schemas, i.e.,
    different column names. You have two options.

    **Option 1:** Check the data and fix the schema. Explore the reason
    why the data have different schemas. Remove wrong files. Change
    column names. And try again.

    **Option 2:** If you know what you are doing, pass argument
    `enforce_schema = FALSE` to `read_repertoires`. The resultant table
    will have NAs in the place of missing values. But don't use it
    without considering the first option. Broken schema usually means
    that there are some issues in the how the data were processed
    upstream.

10. **Q: `immundata` is too verbose, I'm tired of all the messages. How
    to turn them off?**\
    A: Run the following code
    \``options(rlib_message_verbosity = "quiet")`\` to turn off
    messages.

11. **Q: I don't want to use `pak`, how can I use the good old
    `install.packages`?**

    A: Nothing will stop you, eh? You are welcome:

    ``` r
    # Release
    install.packages("immundata")

    # Pre-release
    install.packages(c("devtools", "pkgload"))
    devtools::install_github("immunomind/immundata-rlang")
    devtools::reload(pkgload::inst("immundata"))

    # Dev version
    devtools::install_github("immunomind/immundata-rlang", ref = "dev")
    devtools::reload(pkgload::inst("immundata"))
    ```
