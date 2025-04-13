---
editor_options: 
  markdown: 
    wrap: 72
---

# immundata-rlang

## Installation

### Latest CRAN release

``` r
install.packages("immundata")
```

### Latest GitHub release

If you want the very latest release, or if the above command doesn't
work for some reason, try installing `immundata` from the code
repository:

``` r
install.packages(c("devtools", "pkgload")) # skip this if you already installed these packages
devtools::install_github("immunomind/immundata-rlang")
devtools::reload(pkgload::inst("immundata"))
```

## Latest pre-release

Since releasing on CRAN is limited to one release per one or two months,
you can install the latest pre-release version with all the bleeding
edge and optimised features directly from the code repository. In order
to install the latest pre-release version, you need to execute the
following commands:

``` r
install.packages(c("devtools", "pkgload")) # skip this if you already installed these packages
devtools::install_github("immunomind/immundata-rlang", ref = "dev")
devtools::reload(pkgload::inst("immundata"))
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

## Input / output

### Supported formats

parquet etc.

### Read one or multiple AIRR files into `immundata`

Suppose you have several files. How to read them?

#### 1. Pass a singular file name

#### 2. Pass a vector of file names

#### 3. Pass a glob of files

### Working with the repertoire metadata file

`immundata` modularizes different parts to make sure ??? (modularity /
one big function is bad). Henceforth, `immundata` splits the repertoire
dataset loading into three steps:

1.  Optionally, load the metadata via `load_metadata`

2.  Load the repertoire files from the disk via `load_repertoires` and
    convert them into `immundata` files.

3.  Load the ImmunData files from the converted files via
    `load_immundata` as the final step of `load_repertoires`.

After converting the files to the `immundata` format, you can load them
directly with `load_immundata`.

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

#### Immunogenicity -- annotations from external tools

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

#### Filter by receptors

`filter_receptors`

one, several, regex, edit distance

#### Filter by annotation

`filter_annotations`

one, several values, several columns

#### Filter by barcodes

`filter_barcodes`

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

`annotate_barcodes`

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
    `enforce_schema = FALSE` to `load_repertoires`. The resultant table
    will have NAs in the place of missing values. But don't use it
    without considering the first option. Broken schema usually means
    that there are some issues in the how the data were processed
    upstream.

10. `immundata` is too verbose, I'm tired of all the messages. How to
    turn them off?\
    \
    A: Run the following code
    \``options(rlib_message_verbosity = "quiet")`\` to turn off
    messages.
