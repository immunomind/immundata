[![CRAN](http://www.r-pkg.org/badges/version-ago/immundata?style=flat-square)](https://cran.r-project.org/package=immundata)
[![Downloads_all](http://cranlogs.r-pkg.org/badges/grand-total/immundata?style=flat-square)](https://www.r-pkg.org/pkg/immundata)
[![Downloads_week](http://cranlogs.r-pkg.org/badges/last-week/immundata?style=flat-square)](https://www.r-pkg.org/pkg/immundata)
[![Issues](https://img.shields.io/github/issues/immunomind/immundata-rlang?style=flat-square)](https://github.com/immunomind/immundata-rlang/issues)

# 🦋 `immundata` – A unified data layer for large-scale single-cell, spatial and bulk immunomics in R

`immundata` introduces the `ImmunData` data structure – think [AnnData](https://github.com/scverse/anndata) or [SeuratObject](https://github.com/satijalab/seurat-object/) but for immune repertoires – so you can:

- Store tens of millions of immune receptors plus metadata in one place;

- Compute receptor- and repertoire-level statistics leveraging single-cell, spatial, immunogenicity or any other annotations;

- Work seamlessly with datasets that don't fit in memory;

- Run the same workflow on a laptop, server, or cloud instance.

---

## 🤔 Why `immundata`?

Modern immunomics no longer ends at a couple of FASTQ files and a bar plot:

- We now blend bulk AIRR-seq, single-cell V(D)J + GEX, spatial transcriptomics, clinical metadata and public databases – often inside the same analysis notebook;

- Pipelines that handle gigabytes today face deca-gigabytes after the next experiment;

- The same immune repertoire dataset must power multiple plots, dashboards, deep learning models and be reproducible months (years, ideally) later.

`immundata` is the data-engineering backbone powered by [Arrow](https://arrow.apache.org/docs/r/), [DuckDB](https://duckdb.org/), and [duckplyr](https://duckplyr.tidyverse.org/). It lets you scale, mix and, ultimately, analyse annotated AIRR data without rewriting your biology workflow from scratch each time the dataset grows 10×.

---

> [!IMPORTANT]
> This README is huge. Please consider using navigation.

- 🤔 [Why `immundata`?](#-why--immundata)
- 📦 [Installation](#-installation)
- ⚡ [Quick Start](#-quick-start)
- 🧬 [Workflow Explained](#-workflow-explained)
- 💾 [Ingestion](#-ingestion)
  - [Load AIRR data](#load-airr-data)
  - [Working with metadata table files](#working-with-metadata-table-files)
  - [Receptor schema](#receptor-schema)
  - [Repertoire schema](#repertoire-schema)
  - [Pre‑ and post‑processing strategies](#pre--and-post‑processing-strategies)
  - [Managing output & intermediate ImmunData files](#managing-output--intermediate-immundata-files)
  - [Writing ImmunData objects to disk](#writing-immundata-objects-to-disk)
- 🛠 [Analysis](#-analysis)
  - [Filter](#filter)
  - [Annotate](#annotate)
  - [Compute](#compute)
- 🧩 [Use Cases](#-use-cases)
  - [Bulk -- RepSeq, AIRRSeq](#bulk---repseq-airrseq)
  - [Paired-chain -- scVDJseq or other technologies](#paired-chain---scvdjseq-or-other-technologies)
  - [Single-cell -- scRNAseq, scVDJseq, scTCRseq, scBCRseq](#single-cell---scrnaseq-scvdjseq-sctcrseq-scbcrseq)
  - [Spatial -- spatial transcriptomics and cell coordinates](#spatial---spatial-transcriptomics-and-cell-coordinates)
  - [Annotate immune receptors using external AIRR databases](#annotate-immune-receptors-using-external-airr-databases)
  - [Immunogenicity -- run external tools such as TCRdist to annotate ImmunData](#immunogenicity----run-external-tools-such-as-tcrdist-to-annotate-immundata)
  - [Hybrid datasets](#hybrid-datasets)
    - [Multi-locus data](#multi-locus-data)
    - [Multiple contigs for TCR](#multiple-contigs-for-tcr)
    - [BCR-heavy chains with multiple light chains](#bcr-heavy-chains-with-multiple-light-chains)
    - [Bulk and single-cell data integration](#bulk-and-single-cell-data-integration)
- 🧠 [Advanced Topics](#-advanced-topics)
- 🏷 [About](#-about)
  - [Citation](#citation)
  - [License](#license)
  - [Author and contributors](#author-and-contributors)
  - [Commercial usage](#commercial-usage)
- 🤔 [FAQ](#-faq)

---

> [!WARNING]
> `immundata` is still in the **0.x** series. Until we reach 1.0.0, breaking changes may appear in any minor/patch update (e.g. 0.2.1 → 0.3.0). When you attach the package, sometimes you'll see startup messages summarising
> the most important changes – please read them. If something that used to work suddenly fails, check the updated
> documentation (`?function_name`) first.
>   
> **Tip:** if your analysis depends on a specific behaviour, pin the
> exact version with `renv` or use `pak` for installation:
> ```r
> pak::pkg_install("immunomind/immundata@0.2.1")
> ```  
> I'll keep publishing tagged releases with full docs so you can always
> roll back if needed.


## 📦 Installation

### Prerequisites

Before installing any release or pre-release version of `immundata`, please install `pak` that will simplify the installation of any package, not just `immundata`:

```r
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
```

More info if needed is available on [pak website](https://pak.r-lib.org/#arrow_down-installation).

### Install the latest version

To install the latest release of `immundata`, simply run:

```r
pak::pkg_install("immunomind/immundata-rlang")
```

Mind that this will install the package from our GitHub instead of CRAN. This method is much preferred due to limitations of CRAN and reliance on other packages, which are distributed via `pak` as well.

### Other installation options

We will periodically release `immundata` on CRAN. To install it from CRAN, run 

```r
pak::pkg_install("immundata")
```

If you are willing to try unstable yet bleeding edge features, or if there are some hot fix for your open GitHub ticket, please install the development version:

```r
pak::pkg_install("immunomind/immundata-rlang@dev")
```

---

## ⚡ Quick Start

> [!TIP]
> Interested in specific use cases, e.g., 
> analyse cell clusters from single-cell data,
> work with paired-chain data,
> search for matches in databases with disease-associated TCR and BCR data?
> 
> Take a look at the [🧩 Use Cases section](#-use-cases) below.


Use the immune repertoire data packaged with `immundata` for quick dive.
Replace `system.file` calls with your local file paths to run the code on your data.

```r
library(immundata)

# Metadata table with additional sample-level information
md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")

# Two sample files
samples <- c(
  system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata"), 
  system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
  )

# Read the metadata table
md <- read_metadata(md_path)

# Pass the file paths and the metadata table to the function to read the dataset into R
imdata <- read_repertoires(path          = samples,
                           schema        = c("cdr3_aa", "v_call"),
                           metadata      = md,
                           output_folder = "./immundata-quick-start")

# Print the resultant object in the detailed yet manageable format
imdata

# Check the folder immundata created - this is where your dataset resides now
list.files("./immundata-quick-start")

# Read sections below for data analysis
```

---

## 🧬 Workflow Explained

`immundata` splits the workflow into two clear phases:

1. **Ingestion** – convert your AIRR files into a special format saved on disk, and then read them to a tidy `immundata::ImmunData` object  

2. **Analysis**  – explore, annotate, filter and compute on that object

Before we go into more details for each of the phase, there are three straightforward yet essential `immundata` concepts to keep in mind. These concepts set it apart from data-frame-based AIRR libraries. By extension, the concepts affect how you would work with and even *think* about the data analysis in other packages such as `immunarch` which use `immundata` as a backbone for computations.

### Concepts

1. **Units: chain -> barcode -> receptor**

    | Term               | In plain English                                                                                         | How **immundata** represents it                                                                                                             | **Role**                                                              |
    | ------------------ | -------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- |
    | **Chain**          | A single V(D)J transcript (e.g. *TRA* or *IGH*) coming from one read or contig.                          | One row in the physical table `idata$annotations`; retains `locus`, `cdr3`, `umis`/`reads` and other crucial rearrangement characteristics. | **Raw data unit** – atomic building block.                            |
    | **Barcode / Cell** | The droplet (10x), spot (Visium) or well a chain was captured in.                                        | Column `imd_barcode`.                                                                                                                       | **Physical bundle** – groups chains that share a capture compartment. |
    | **Receptor**       | The biological receptor you analyse: a single chain **or** a paired set (αβ, Heavy-Light) from one cell. | Virtual table `idata$receptors`; unique ID `imd_receptor_id`.                                                                               | **Logical unit** – minimal object for AIRR statistics.                |
    | **Repertoire**     | A set of receptors grouped by sample, donor, cluster, etc.                                               | Physical table `idata$repertoires`; unique ID `imd_repertoire_id`; grouping columns you choose.                                             | **Aggregate unit** – higher-level grouping for comparative analysis.  |

    **Chain** is one V(D)J rearranged molecule / contig / chemistry read (e.g. a single TRA, TRB, IGH, IGL). This is a minimally possible data unit, a building block of everything.
    In case of single-chain data, chain is the same as barcode. Never changes after ingest; you can always drill back to the exact sequence.
    
    **Barcode** is a physical container that stores zero, one, or many chains. In single‑cell it's a droplet == cell; in bulk it's the same as assembled "clonotype"; in spatial it's a spot. Barcode sometimes equals to cell. It is a biological unit that "stores" relevant biological data and uses for aggregation of same chains and computing counts of same receptors coming from different barcodes. Inherits any per‑cell / per‑sample metadata you add.
    
    **Receptor** is a logical unit. A logical grouping of chains that you want to treat as one biological receptor. This is a minimal unit for AIRR statistics. There are two components to it: receptor features and receptor chains, alltogether comprising a receptor schema that you define in order to do downstream analysis. Receptor feautres are usually CDR3 amino acid sequences or CDR3 amino acid sequences plus Variable gene segment. Receptor chains can be: single chain, α+β pair, heavy+light pair, or even all chains sharing the same CDR3/V/J.
    
    To summarise: chains are how `immundata` stores the information, barcodes bundle chains together, and receptors are the minimal units on which repertoire statistics are computed.

2. **Aggregation: defining receptors and repertoires**

    The moment data leave an AIRR-assembly tool such as **Cell Ranger**, you are handed an ocean of individual V(D)J chains, yet every biological question you care about is phrased in terms of receptors ("this αβ TCR") or repertoires ("all receptors from donor A on day 30"). As with "receptors as logical units", the underlying assumption the second concept is based upon is *researchers work with rearrangements but think in receptors*. `immundata` formalises the climb from raw chains to those higher-order concepts through **controlled aggregation** – explicit, user-defined rules that transform data without obscuring their origin.

    The function `agg_receptors()` lets you declare what *one receptor* means in your study. You choose a schema – perhaps "pair chains that share a barcode and have complementary α and β loci" or "group every IGH with whatever IGL shares the same CDR3 amino-acid sequence." The function re-aggregates the data and returns a new `ImmunData` object, so you keep the previous receptor definition intact; every receptor now has a stable identifier and can be traced back to its constituent chains and barcode. There is no need to touch the downstream pipeline – just change the input.

    The function `agg_repertoires()` states how receptors should be bundled into biologically meaningful cohorts: all receptors from a biopsy, from a therapy responder, from a single-cell-defined cluster, or any combination of metadata columns. The result is a physical `idata$repertoires` table with basic statistics (numbers of chains, barcodes, and unique receptors), again preserving direct links to the receptors it aggregates.

    Because these aggregation steps live in your pipeline rather than being buried inside helper functions, they deliver two major pay-offs:

    - **Convenience with rigour:** you can run high-level computations – Jaccard coefficients, diversity indices – knowing that the exact receptor definition is stored alongside the result, so you never mis-specify parameters such as `"cdr3+v"`;
    
    - **Provenance and data lineage by design:** every receptor records every chain it contains, every repertoire records every receptor, and the full recipe is stored in the object's metadata. Six months later – or six reviewers later – you can trace any summary statistic back to the precise chains that produced it, enabling fully reproducible pipelines with no hidden transformations.

3. **Pipeline-based execution: immutability and materialisation**

    The explicit data lineage we talked about in concepts 1 & 2 pays its dividend only if every step is re-playable. That is why `immundata` treats an analysis as a *pipeline of immutable transformations*. Each function returns a fresh `ImmunData` object, leaving the parent untouched; the full chain of objects records how the data travelled from raw chains to final statistics.

    Because immunomics datasets started to regularly outgrow RAM, those objects do **not** live in memory by default. Their tables are persisted as column-compressed Parquet files and "materialised" – pulled into RAM – only when a computation truly needs them, typically to crunch a subset or to emit the final numbers such as repertoire-overlap indices. For a 10 GB dataset that fits in memory, this behaviour is invisible: DuckDB streams the file, you get an in-memory frame, and life goes on. For a 100 GB experiment, the same code still runs; the heavy joins spill to disk, and the intermediate results are cached so downstream steps can reuse them without recomputation.
    
    Thinking in pipelines therefore means two things:
    
    - **Cache what matters:** create intermediate `ImmunData` objects when you hit an expensive step, and write them to disk; the next run can pick up from there. A prime example of this a computing edit distances to some patterns or sequences.
    
    - **Assume re-execution:** any colleague (or future-you on a bigger cluster) should be able to rerun `pipeline.R` end-to-end without interactive tinkering and arrive at the same result byte-for-byte.
    
    All of this engineering should stay behind the curtain. Downstream packages that adopt `immundata` as their backbone should expose high-level verbs such as `compute_diversity()` or `plot_overlap()`; the user need not touch `ImmunData`, DuckDB, or Parquet. In the ideal scenario they never learn that an on-disk database powers their workflow – and they never have to.
    
    Leave the data engineering to the data engineers (and, sadly, bioinformaticins – I feel your pain); keep your focus or the focus of your users on the biology. It is sophisticated enough already.

And now, let's dive into how you work with `immundata`.

### Phase 1: Ingestion

```
      ┌───────┐
      │ files │
      └───────┘
          │
          ▼
   read_metadata()    ──── Read metadata
          │
          ▼ 
  read_repertoires()  ──┬─ Read repertoire files (!)
          │             │      ▼
          │             │  Preprocess
          │             │      ▼
          │             │  Aggregate receptors (!)
          │             │      ▼
          │             │  Postprocess
          │             │      ▼
          │             │  Aggregate repertoires 1
          │             │      ▼
          │             └─ Write data on disk (!)
          ▼
   agg_repertoires()  ──── Aggregate repertoires 2
          │
          ▼
    ┌───────────┐
    │ ImmunData │
    └───────────┘
```

Steps marked with `(!)` are non-optional.

  1) **Read metadata:**
  
      This step loads any sample‑ or donor‑level info to your environment using `read_metadata()` function.
    
      This step is optional. You can safely skip it if you don't have per-sample pr per-donor metadata, such as therapy response, HLA, age, etc. But it is highly recommended. See an example of metadata file below in the "Ingestion" section.
  
  2) **Read repertoire files:**
  
      Parquet/CSV/TSV → DuckDB tables via `read_repertoires()`.
  
  3) **Preprocess:**
  
      Drop unwanted cols, drop nonproductive sequences, rename to AIRR schema, remove duplicate contigs by passing the preprocessing strategy to `read_repertoires()`.
  
      Optional yet recommended step, turned on by default.
  
  4) **Aggregate receptors:**
  
      Collapse by CDR3/V/J (or your schema) by passing a receptor schema to `read_repertoires()`.
      
  3) **Postprocess:**
  
    ...
  
  5) **Aggregate repertoires 1:**
  
      Group per sample/donor/time to define set of receptors or repertoires - either inside `read_repertoires()` if you pass a repertoire schema to it, or separately by calling `agg_repertoires()` function.
  
      Optional step, you can define repertoires later using more information, e.g., in the single-cell case, first, you import cell cluster information, and second, you define repertoires using the donor + cluster information.
      
  3) **Write data on disk:**
  
    ...
    
  5) **Aggregate repertoires 2:**
  
    ...

### Phase 2: Analysis

```
      ┌───────────┐       ┌────────────────────────────┐
      │ ImmunData │       │ AnnData / Seurat / TCRdist │
      └───────────┘       │ seur@meta.data / adata.obs │
            │             └────────────────────────────┘
            │                             │
            ├─────────────────────────────┘
            │
            ▼
   annotate_immundata()    ──── Import external annotations to ImmunData
            │
            ▼ 
     agg_repertoires()     ──── Aggregate repertoires
            │
            ▼ 
    filter_immundata()     ──── Filter receptors or repertoires
            │
            ▼ 
    mutate_immundata()     ──── Compute statistics or transform ImmunData
            │
            │     ┌───────────────┐
            ├────►│ save / plot 1 │
            │     └───────────────┘
            ▼ 
   annotate_immundata()    ──── Annotate ImmunData with the computed statistics
            │
            │     ┌───────────────┐
            ├────►│ save / plot 2 │
            │     └───────────────┘
            │
            ▼
┌────────────────────────┐
│seur@meta.data[:] <- ...│ ──── Export ImmunData annotations
│    adata.obs = ...     │
└────────────────────────┘
```

  1) **Import external annotations to ImmunData:**
  
      `annotate_cells` from the single-cell data
  
  2) **Aggregate repertoires:**
  
      Optionally aggregate to repertoires, potentially using the newly annotate data.
  
  3) **Filter receptors or repertoires:**
  
      `filter_immundata` gets you identifiers of interest and their corresponding receptor features, potentially using the annotation from the previous step
  
  4) **Compute statistics or transform ImmunData:**
  
      On this step, you compute statistics per-repertoire or per-receptor, using input receptor features. There are several scenarios depending on what you try to achieve.
  
      1) use `immunarch` for the most common analysis functions. The package will automatically annotate both *receptors/cells* (!) and *repertoires* (!!) if it is possible.
  
      2)  simply mutate on the whole dataset using `dplyr` syntax, like compute the number of cells or whatever
  
      3) more complex compute that requires a function to apply to values and is probably not supported by `duckplyr`. See the [🧠 Advanced Topics](#-advanced-topics) for more details.
      
  4) **save / plot 1:**
  
    ...
  
  5)  **Annotate ImmunData with the computed statistics:**
  
      `annotate_immundata` joins the computed values back to the initial dataset using the identifiers. If you already have identifiers, you can simply use `annotate_cells` or `annotate_receptors`.
  
  4) **save / plot 2:**
  
    ...
  
  6)  **Export ImmunData annotations:**
  
      Writes the annotated data back to the cell-level dataset (Seurat / AnnData) for the subsequent analysis. Additionally, you could write the immundata itself to disk if needed.

---

## 💾 Ingestion

### Load AIRR data

`immundata` provides a flexible system for loading immune receptor repertoire files from different sources -- CSV, TSV and Parquet files, possibly gzipped, with some optionality. The main function for this is `read_repertoires()`. Below are four ways to pass your file paths and one for convering data from existing `immunarch pre-1.0` list objects with `$data` and `$meta`.

  1. **Pass a single file name:**
  
      If you just have **one** AIRR file:

      ```r
      library(immundata)
      
      inp_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
      
      idata <- read_repertoires(
        path   = inp_file,
        schema = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

  2. **Pass a vector of file names:**
  
      For **multiple** files in a vector:

      ```r
      library(immundata)
      
      inp_file1 <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
      inp_file2 <- system.file("extdata/tsv", "sample_1k_2k.tsv", package = "immundata")
      
      file_vec <- c(inp_file1, inp_file2)
      
      idata <- read_repertoires(
        path   = file_vec,
        schema = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

      `immundata` automatically merges them (depending on your chosen schema), writes the aggregated data into a single directory of Parquet files, and produces a single-cell `ImmunData` object. Think about it as a huge table instead of smaller multiple repertoire tables.

  3. **Pass a glob pattern:**
  
      If your files follow a consistent naming pattern, you can leverage shell globs:

      ```r
      library(immundata)
      
      folder_with_files <- system.file("extdata/tsv", "", package = "immundata")
      
      glob_files <- paste0(folder_with_files, "sample*.tsv")
      
      print(glob_files)
      # The output is something like "/Library/Frameworks/.../immundata/extdata/tsv/*"
      # Mind the star "*" at the end
      
      # For example, all AIRR files in the 'samples/' folder
      idata <- read_repertoires(
        path   = glob_files,
        schema = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

      Behind the scenes, `read_repertoires()` expands the glob with `Sys.glob(...)`, merges the data, and produces a single `ImmunData`.

  4. **Use a metadata file:**
  
      Sometimes you need more control over the data source (e.g. consistent sample naming, extra columns). In that case:

        1.  **Load metadata** with `read_metadata()`.
        
        2.  **Pass** the resulting data frame to `read_repertoires(path = "<metadata>", ..., metadata = md_table)`. Mind the `"<metadata>"` string we pass to the function. It indicates that we should take file paths from the input metadata table.

      An example code:

      ```r
      library(immundata)
      
      md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
      
      md_table <- read_metadata(md_path)
      
      print(md_table)
      ```
      
      ```
      # The column "File" stores the file paths. If you have a different column name
      # for files, use the `metadata_file_col = "<your column name>"` argument.
      # A tibble: 2 × 5
        File                       Therapy Response Prefix filename
        <chr>                      <chr>   <chr>    <chr>  <chr>   
      1 /.../immundata-/inst/extd… ICI     FR       S1_    /Users/…
      2 /.../immundata-/inst/extd… CAR-T   PR       S2_    /Users/…
      ```
      
      ```r
      idata <- read_repertoires(
        path     = "<metadata>",
        metadata = md_table,
        schema   = c("cdr3_aa", "v_call")
      )
      
      print(idata)
      ```

      This approach **unifies** sample-level metadata (e.g. donor ID, timepoint) with your repertoire data inside a single `ImmunData`.
      
      You can pass the metadata table separately along with the list of files as we did in the previous examples without the "<metadata>" directive, but in that case you would need to check the correctness of all filepaths by yourself. Which could be quite cumbersome, to say the least.
      
      The more information on how to work with metadata files, please read the next section.

  5. **Convert from `immunarch` lists:**
  
      Pass `immunarch` data lists to `from_immunarch()` to create `ImmunData` objects.
  
      ```r
      library(immundata)
      # Install old immunarch:
      # pak::pkg_install("immunomind/immunarch@0.9.1")
      data(immdata, package = "immunarch")
      
      idata <- from_immunarch(
        imm = immdata, 
        schema = c("CDR3.aa", "V.name"), 
        output_folder = "./immdata-test")
        
      print(idata)
      ```

### Working with metadata table files

Metadata tables store the sample-level information. When `immundata` loads the metadata, it annotates every receptor from a given sample (or file) with the corresponding metadata fields. For example, if a sample has "Therapy" = "CAR‑T", all receptors from that sample receive the same "Therapy" value. You can then aggregate receptors by donor, tissue, or any other field and run your analysis on those repertoires (see the next sections for aggregations).

```r
library(immundata)

md_path <- system.file("extdata/tsv", "metadata.tsv", package = "immundata")
md_table <- read_metadata(md_path)
```

```
Rows: 2 Columns: 4
── Column specification ─────────────────────────────────────────────────────────
Delimiter: "\t"
chr (4): File, Therapy, Response, Prefix

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
ℹ Found 2/2 repertoire files from the metadata on the disk
✔ Metadata parsed successfully
```

```r
print(md_table)
```

```
# A tibble: 2 × 5
  File                       Therapy Response Prefix filename
  <chr>                      <chr>   <chr>    <chr>  <chr>   
1 /.../immundata-/inst/extd… ICI     FR       S1_    /Users/…
2 /.../immundata-/inst/extd… CAR-T   PR       S2_    /Users/…
```

### Receptor schema

`immundata` lets you decide what a receptor means for your study by specifying:

 - Feature columns - which fields make a receptor unique. Usually it is `"cdr3_aa"` and `"v_call"` columns.

 - Chains to keep / pair - e.g. TRA only or a pair TRA + TRB.

If you have only feature columns, you can usually pass the character vector with columns to functions. In a more advanced case with multiple chain data, `immundata` provides a helper function `make_receptor_schema` for building schemas:

```r
schema <- make_receptor_schema(
  features = c("cdr3_aa", "v_call"),
  chains   = c("TRA", "TRB")
)
```

  1. **Chain-agnostic**

      Used for bulk or pre-filtered immune repertoire data. No filtering by chain data such as TRA or TRB. Each unique combination of features in the schema vector is assigned a unique receptor identifier and counts as a receptor. In the example below, the receptor features are "cdr3_aa" and "v_call" columns - CDR3 amino acid sequence and V gene segment columns respectively.
      
      ```r
      library(immundata)
            
      inp_file <- system.file("extdata/tsv", "sample_0_1k.tsv", package = "immundata")
      
      schema <- c("cdr3_aa", "v_call")
            
      idata <- read_repertoires(
        path   = inp_file,
        schema = schema
      )
            
      print(idata)
      ```

  2. **Single-chain**

      Used for paired-chain data such as single-cell data to focus on the analysis of immune receptors with a specific chain. The data is pre-filtered to leave the data units with the specified chain only.
      
      ```r
      library(immundata)
            
      inp_file <- system.file("extdata/single_cell", "lt6.csv.gz", package = "immundata")
      
      schema <- make_receptor_schema(
        features = c("cdr3", "v_call"),
        chains   = "TRA"
      )
      
      idata <- read_repertoires(
        path        = inp_file,
        schema      = schema,
        barcode_col = "barcode",
        locus_col   = "locus",
        preprocess  = make_default_preprocessing("10x")
      )
      
      print(idata)
      ```

  3. **Paired-chain**

      When you want full αβ (or heavy‑light) receptors, immundata can pair two chains that originate from the same barcode and keep, for each locus, the chain with the highest UMI/reads. A single unique receptor identifier is then assigned to the pair. The data is pre-filtered to loci in target chains. Within each barcode×locus the the chain with max umis or reads is selected. Barcodes lacking either chain are dropped from the receptor table.
      
      ```r
      library(immundata)
      
      inp_file <- system.file("extdata/single_cell", "lt6.csv.gz", package = "immundata")
      
      schema <- make_receptor_schema(
        features = c("cdr3", "v_call"),
        chains   = c("TRA", "TRB")
      )
      
      idata <- read_repertoires(
        path        = inp_file,
        schema      = schema,
        barcode_col = "barcode",         # required for pairing
        locus_col   = "locus",           # column that says "TRA" / "TRB"
        umi_col     = "umis",            # choose chain with max UMIs per locus
        preprocess  = make_default_preprocessing("10x")
      )
      
      print(idata)
      ```

Cheat-sheet for arguments to `read_repertoires`:

| Situation                                | `barcode_col` | `locus_col` | `umi_col` | `chains`         |
| ---------------------------------------- | ------------- | ----------- | --------- | ---------------- |
| Bulk data, no locus filtering            | no            | no          | no        | omit / `NULL`    |
| Analyse TRA only                         | optional¹     | **yes**     | no        | `"TRA"`          |
| Pair TRA+TRB, pick best chain per cell   | **yes**       | **yes**     | **yes**   | `c("TRA","TRB")` |

¹ If you pass barcodes, they're stored but not used for pairing.

### Repertoire schema

> [!CAUTION]
> 🚧 Under construction. 🚧

To compute repertoire‑level statistics such as gene‑segment usage, the Jaccard coefficient, or the incidence of public receptors, you first need to define a repertoire. In `immundata` a repertoire is simply a group of receptors that share one or more values from annotation columns.

Just like with receptors, you can pass a schema - a character vector of column names - to specify how receptors are grouped into repertoires.

For the bulk data, usually, you rely on the metadata table. It could be useful when you want to aggregate together receptors from the same donor or tissue, and then analyse it. Or you may want to filter out non-responders to analyse the responders only.

```r
```

> [!NOTE]
> Don't confuse grouping of immune repertoires with grouping in plots.
> When you define an immune repertoire, all the proportions are recomputed, and each receptor assigned
> a unique repertoire identifier for faster computations.
> You create virtual "tables" with immune receptors, and you can work with them separately
> using filters or mutations, despite that the data is still stored as a huge table with all receptors.
> When you plot data, you first compute statistics per defined immune repertoire, and then you
> group it. You can later plot or re‑group the resulting statistics, but the order of operations matters.

The true power of regrouping repertoires opens up when you work with single-cell data.

```r
```

### Preprocessing and postprocessing strategies 

> [!CAUTION]
> 🚧 Under construction. 🚧

-   filtering non productive
-   double contigs
-   double BCR chains
-   locus

### Managing the output and intermediate ImmunData files

> [!CAUTION]
> 🚧 Under construction. 🚧

By default, `read_repertoires()` writes the created Parquet files into a directory named `immundata_<first filen name>`. Consider passing `output_folder` to `read_repertoires` if you want to specify the output path.

### Writing ImmunData objects on disk

> [!CAUTION]
> 🚧 Under construction. 🚧
 
Use `write_immundata` to save ImmunData objects.

Why you might need it - to save intermediate files, e.g., after computing levenshtein distance to specific receptors from the database. In that case, you wouldn't need to recompute the distances each time.

---

## 🛠 Analysis

### Filter

> [!CAUTION]
> 🚧 Under construction. 🚧

The key function for this is `filter` (`dplyr`-compatible) or `filter_immundata`.

  1. **Filter by any annotation**
  
      ???
      
  2. **Filter by receptor features or their identifiers**
  
      ???
      
  3. **Filter by barcodes**
  
      ???

receptor features or their identifiers 

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

### Annotate

> [!CAUTION]
> 🚧 Under construction. 🚧

The key function for this is `annotate` and `annotate_immundata`.
      
  1. **Annotate by any metadata**
  
      ???
      
  2. **Annotate by receptor features or identifiers**
  
      ???
      
  3. **Annotate by barcodes**
  
      ???

### Compute

The key functions for this is `mutate` (`dplyr`-compatible) / `mutate_immundata` and the functions from the downstream analysis tools.

  1. **Add or transform one or several annotation columns**
  
      ???

  2. **Basic repertoire statistics using `immundata`**

      Find receptors from several repertoires or groups which have >2 abundance

  3. **Basic analysis using `immunarch`**

      ???

---

## 🧩 Use Cases

### Bulk -- RepSeq, AIRRSeq

> [!CAUTION]
> 🚧 Under construction. 🚧

if you use immunarch or so, you probably already have a metadata. If you don't better create it

### Paired-chain -- scVDJseq or other technologies

> [!CAUTION]
> 🚧 Under construction. 🚧

It works the same for any data, including bulk and single-cell - just pass a schema

### Single-cell -- scRNAseq, scVDJseq, scTCRseq, scBCRseq

> [!CAUTION]
> 🚧 Under construction. 🚧

-   load annotation data
-   do something
-   write the annotation data back
-   visualize AIRR with annotations data
-   visualize SC with annotation data

For more information see the vignette tutorial: 

- run `vignette("single_cell")` from R, or
- follow the link to read it online: [link](link).

### Spatial -- spatial transcriptomics and cell coordinates

> [!CAUTION]
> 🚧 Under construction. 🚧

### Annotate immune receptors using external AIRR databases 

> [!CAUTION]
> 🚧 Under construction. 🚧

### Immunogenicity -- run external tools such as TCRdist to annotate ImmunData 

> [!CAUTION]
> 🚧 Under construction. 🚧

### Hybrid datasets 

#### Multi-locus data

> [!CAUTION]
> 🚧 Under construction. 🚧

#### Multiple contigs for TCR

> [!CAUTION]
> 🚧 Under construction. 🚧

#### BCR-heavy chains with multiple light chains

> [!CAUTION]
> 🚧 Under construction. 🚧

#### Bulk and single-cell data integration

> [!CAUTION]
> 🚧 Under construction. 🚧

### Receptor clusters and motifs

> [!CAUTION]
> 🚧 Under construction. 🚧

---

## 🧠 Advanced Topics 

### Developers

#### Integrate into your package 

`immundata` is created in mind with the mission of replacing typical data frame-based formats, usually not following the AIRR-C file standard.

#### Extend with functions

S3 methods etc.

### How `immundata` reads the data

By design, **`immundata`** data-loading pipeline is **three** steps, rather than one giant function. This promotes modularity, easier debugging, and flexible usage:

1.  **(Optionally) Load the metadata** via `read_metadata()`.
    -   This ensures your metadata has the correct file paths, absolute or relative.
2.  **Load the repertoire files** from disk via `read_repertoires()`.
    -   This function unifies your data (be it 1 file or 100 files) and **outputs** two Parquet files:
        -   **`receptors.parquet`** (receptor-level aggregation)
        -   **`annotations.parquet`** (cell-level data, sample metadata, etc.)
    -   It then calls `read_immundata()` to return a fully instantiated `ImmunData` object that uses the newly created files on the disk as a source. The helps two-fold: you don't lose your data, and it allows `immundata` to run an optimized code when necessary.
3.  **(Optionally) Load the same `ImmunData` files later** with `read_immundata()`.
    -   If you need to reopen the data in a future R session, you don't have to redo the entire pipeline.
    -   Just call `read_immundata(path_to_immundata_folder)` where the folder contains `receptors.parquet` and `annotations.parquet`.
    -   With this approach, you **never** need to re-parse your raw AIRR files once you've generated the Parquet-based `immundata` format.

Why split it up?

-   **Modularity**: If something breaks, you can debug whether it's in metadata parsing or the actual repertoire table creation.
-   **Reusability**: It is straightforward to share one folder with two `immundata` files.
-   **Performance**: Once your data is in `immundata` format, you can load it in future sessions in **constant time** without merging or parsing again.

### Custom functions for analysis

1) Function is supported by `duckdb` - then use `dd$<function_name>`

2) Use SQL

3) Run a completely custom function

### Change RAM limits to accelerate the backend computations 

TODO

### Make `immundata` even faster with data engineering tricks

TODO - resave the data to use hive partitioning + show benchmarks

### Save your intermediate data for faster computations and reproducibility 

TODO

---

## 🏷 About 

### Citation 

TODO

### License 

TODO

### Author and contributors 

TODO

### Commercial usage 

`immundata` is free to use for commercial usage. However, corporate users will not get a prioritized support for `immundata`- or AIRR-related issues. The priority of open-source tool `immundata` is open-source science.

If you are looking for prioritized support and setting up your data pipelines, consider contacting Vadim Nazarov for commercial consulting and support options.

Workshops / training sessions / designing data platforms and machine learning systems for multi-omic

---

## 🤔 FAQ 

1.  **Q: Why all the function names or ImmunData fields are so long? I want to write `imdata$rec` instead of `imdata$receptors`.**

    A: Two major reasons - improving the code readability and motivation to leverage the autocomplete tools.

    Please consider using `tab` for leveraging autocomplete. It accelerates things x10-20.

2.  **Q: How does `immundata` works under the hood, in simpler terms?**

    A: `immundata` uses the fantastic `duckplyr` package

    References:

    -   [duckplyr](https://duckplyr.tidyverse.org/index.html)

3.  **Q: Why do you need to create Parquet files with receptors and annotations?**

    A: First of all, you can turn it off. Second, those are intermediate files, optimized for future data operations, and working with them significantly accelerates `immundata`. Take a look at our benchmark page to learn more: `link`

4.  **Q: Why does `immundata` support only the AIRR standard?!**

    A: The practical answer is that `immundata` allows some level of optionality - you can provide column names for barcodes, etc.
    
    The short answer is that we have our own standards already.
    
    The long answer is that the amount of investments required not only for the devlopment, but also for continued support of parsers for different formats is astounishing. I delopved parsers for 10 formats for tcR / immunarch packages, and I would much prefer for upstream tool developers to not change their format each minor versions, breaking pretty much all downstream pipelines and causing all sorts of pain to end users and tools developers - mind you, without bearing a responsibility to at least notify, but ideally fix the broken formats they introduced. The time of Wild West is over - the AIRR community did an outstanding job creating it's standard. Please urge the creators of your favorite tools or your fellow developers to use this format or a superset of it, like immundata does.
    
    Immundata is not and will not support other formats explicitly. This is both a practical stance and communication of crucial values, put into immundata as a part of a broader ecosystem of AIRR tools. The domains is already too complex, and we need to work together to make this complexity manageable.

5.  **Q: Why is it so complex? Why do we need to use `dplyr` instead of plain R?**

    A: The short answer is:

    -   faster computations,
    -   code, that is easy to maintain and support by other humans,
    -   and better data skills,
    -   in most cases you don't really need a complexity, so we can optimize the 95% of all AIRR data operations

    For the long answer, let me give you more details on each of the bullepoint.

6.  **Q: How do I get to use all operations from `dplyr`? `duckplyr` doesn't support some operations, which I need.**

    A: Let's consider several use cases.

    **Case 0.** You are missing `group_by` from `dplyr`.

    **Case 1.** Your data can fit into RAM.

    **Case 2.** Your data won't fit into RAM, and you really need to work on all of this data.

    **Case 3.** Your data won't fit into RAM, but before running intensive computations, you are open to working with smaller dataset first.

7.  **Q: You filter out non-productive receptors. How do I explore them?**

    A: option for saving non-productive chains to a separate file

8.  **Q: Why does `immundata` have its own column names for receptors and repertoires? Could you just use the AIRR format - repertoire_id etc.?**

    A: The power of `immundata` lies in the fast re-aggregation of the data, that allows to work with whatever you define as a repertoire on the fly via `ImmunData$build_repertoires(schema = ...)`

9.  **Q: What do I do with following error: "Error in `compute_parquet()` at [...]: ! ?***

    A: It means that your repertoire files have different schemas, i.e., different column names. You have two options.

    **Option 1:** Check the data and fix the schema. Explore the reason why the data have different schemas. Remove wrong files. Change column names. And try again.

    **Option 2:** If you know what you are doing, pass argument `enforce_schema = FALSE` to `read_repertoires`. The resultant table will have NAs in the place of missing values. But don't use it without considering the first option. Broken schema usually means that there are some issues in the how the data were processed upstream.

10. **Q: `immundata` is too verbose, I'm tired of all the messages. How to turn them off?**
    
    A: Run the following code `options(rlib_message_verbosity = "quiet")` in the beginning of your R session to turn off messages.

11. **Q: I don't want to use `pak`, how can I use the good old `install.packages` or `devtools`?**

    A: Nothing will stop you, eh? You are welcome, but I'm not responsible if something won't work due to issues with dependencies:

    ```r
    # CRAN release
    install.packages("immundata")

    # GitHub release
    install.packages(c("devtools", "pkgload"))
    devtools::install_github("immunomind/immundata-rlang")
    devtools::reload(pkgload::inst("immundata"))

    # Development version
    devtools::install_github("immunomind/immundata-rlang", ref = "dev")
    devtools::reload(pkgload::inst("immundata"))
    ```

12. **Q: Why the counts for receptors are only available after all the aggregation?**

    A: Because the counts and proportions are properties of specific receptors in specific repretoires. The number of chains is the number of reads for bulk sequencing or just one if the cell barcode information is available. When we aggregate data to receptors and repertoires, only then we can count the number of chains - per receptor - per repertoire.
