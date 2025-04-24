[![CRAN](http://www.r-pkg.org/badges/version-ago/immundata?style=flat-square)](https://cran.r-project.org/package=immundata)
[![Downloads_all](http://cranlogs.r-pkg.org/badges/grand-total/immundata?style=flat-square)](https://www.r-pkg.org/pkg/immundata)
[![Downloads_week](http://cranlogs.r-pkg.org/badges/last-week/immundata?style=flat-square)](https://www.r-pkg.org/pkg/immundata)
[![Issues](https://img.shields.io/github/issues/immunomind/immundata-rlang?style=flat-square)](https://github.com/immunomind/immundata-rlang/issues)

# 🦋 `immundata` --- A Unified Data Layer for Large-Scale Single-Cell, Spatial and Bulk Immunomics in R

- 🤔 [Why `immundata`?](#-why--immundata)
- 📦 [Installation](#-installation)
- ⚡ [Quick Start](#-quick-start)
- 🧬 [Workflow Explained](#-workflow-explained)
- 💾 [Ingestion](#-ingestion)
- 🛠 [Analysis](#-analysis)
  - [Filtering](#filtering)
  - [Annotations](#annotations)
  - [Compute statistics](#compute-statistics)
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
> `immundata` is still in the **0.x** series. Until we reach 1.0.0, breaking changes may appear in any minor/patch update (e.g. 0.2.1 → 0.3.0).  
> When you attach the package you’ll see startup messages summarising
> the most important changes—please read them.  
> If something that used to work suddenly fails, check the updated
> documentation (`?function_name`) first.
>   
> **Tip:** if your analysis depends on a specific behaviour, pin the
> exact version with `renv` or  
> ```r
> pak::pkg_install("immunomind/immundata@0.2.1")
> ```  
> I’ll keep publishing tagged releases with full docs so you can always
> roll back if needed.

## 🤔 Why `immundata`?

For modern data engineering and machine learning immunomics applications, which characterise by large-scale pipelines and more robust and complex engineering around.

Modern immunomics no longer stops at a dozen FASTQ files and a bar plot.
Today we build terabyte-scale, ML-ready pipelines that connect bulk AIRR-seq, single-cell V(D)J + GEX, spatial transcriptomics, clinical metadata and public databases --- often inside the same analysis notebook.

immundata is the data-engineering backbone that makes this doable without rewriting your biology workflow each time the dataset grows 10×.

- data keeps bigger
- data complexity - data keeps more complicated with additional metadata
- workflow complexity - complex data require complex workflows
- reproducibility

Thus we build tools that follow the key architectural principles:

- **Scalability**
- **Modularity**
- **Extendability**

- An open-source and modern data engineering and machine learning stack for immunomics-driven development of personalized immunotherapies and vaccines.

- An open-source ecosystem powering the modern data engineering and machine learning applications for immunomics-driven development of personalized immunotherapies and vaccines.

- The modern data engineering and machine learning stack for immunomics-driven development of personalized immunotherapies and vaccines. Open-source ecosystem built by Vadim Nazarov and the community.

- Open-source ecosystem of immune repertoire tools for the development of personalized immunotherapies and vaccines. Built by Vadim Nazarov and the community.

> I design and develop Machine Learning systems for the domains, where uncertainty is high, risks are huge, but the potential gain is enormous. My core focus is on two domains, which are much more connected to each by core principles than people realize:
>
> - Complex LLM systems for the most complex domains of human knowledfge - from healthcare to ...
>
> - Modern data engineering and machine learning platforms for multi-modal multi-omic data - single-cell transcriptomics, immune repertoires, etc.

> The principles they unite are:
Potential Core Principles:
Complexity and Scale: Both require scalable infrastructure and orchestration (Kubernetes, scalable inference systems, distributed computing frameworks).

Data Richness & Multimodality: Both require dealing with diverse data (text, numerical, genomic, spatial), necessitating advanced data engineering and preprocessing.

High-Precision Requirements: Both demand extremely high-quality outputs (hallucination-free LLMs, precision medicine analytics).

Advanced Model Management: Both scenarios involve complex multi-model deployment, monitoring, and evaluation (MLOps, LLMOps).

Compliance and Security: Both require strict compliance—GDPR, HIPAA for LLM health apps; privacy/security for multi-omics data.

Here are refined examples based on your original statement:

“I specialize in building highly scalable and precise machine learning systems for some of humanity’s most complex domains—ranging from advanced multi-omic biomedical analytics (single-cell transcriptomics, immune repertoires) to sophisticated healthcare-focused generative AI systems.”

or simpler:

“I design machine learning and data engineering platforms that excel in the hardest contexts—complex biomedical datasets (single-cell, immunomics) and advanced generative AI (LLMs) in healthcare, wellness, and personalized medicine.”




Stack: Apache Arrow, Parquet, DuckDB, PostgreSQL, Polars, R dplyr, Python ibis


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
md_path <- system.file("extdata", "metadata_samples.tsv", package = "immundata")

# Two sample files
samples <- c(
  system.file("extdata", "sample_0_1k.tsv", package = "immundata"), 
  system.file("extdata", "sample_1k_2k.tsv", package = "immundata")
  )

# Read the metadata table
md <- read_metadata(md_path)

# Pass the file paths and the metadata table to the function to read the dataset into R
imdata <- read_repertoires(samples, c("cdr3_aa", "v_call"), md,
                           output_folder = "./immundata-quick-start")

# Print the resultant object in the detailed yet manageable format
imdata

# Check the folder immundata created - this is where your dataset resides now
list.files("./immundata-quick-start")
```

---

## 🧬 Workflow Explained

`immundata` splits the workflow into two clear phases:

1. **Ingestion** – get your raw files into a tidy `immundata::ImmunData` object  

2. **Analysis**  – explore, annotate, filter and compute on that object

Before we go into more details for each of the phase, there are two simple yet essential `immundata` concepts we need to keep in mind, which distinguish `immundata` from all other data frame-based AIRR libraries.

1. **Units: chain -> barcode -> receptor**

  **Chain** One V(D)J rearranged molecule / contig / chemistry read (e.g. a single TRA, TRB, IGH, IGL). (rearrangement, used "chain" for convenience) - minimally possible data unit, a building block of everything.
  In case of single-chain data, same as barcode. Never changes after ingest; you can always drill back to the exact sequence.
  
  **Barcode** A physical container that stores zero, one, or many chains.
  In single‑cell it’s a droplet == cell; in bulk it’s an entire sample file; in spatial it’s a spot.
  (sometimes equal to cell) - biological unit that "stores" relevant biological data. Inherits any per‑cell / per‑sample metadata you add.
  
  **Receptor** - logical unit. A logical grouping of chains that you want to treat as one biological “receptor signature”.
  Can be: α+β pair, heavy+light pair, or even all chains sharing the same CDR3/V/J.
  Minimal data unit for AIRR statistics.

2. **Aggregation: receptors and repertoires**

  The data stored as chains.
  Biologically-relevant processes are consolidated by barcodes.
  People think in receptors and repertoires.
  
  How do we make it convenient in practical sense?
  
  Data lineage is crucial for full reproducibility.

And now, let's dive into how you work with `immundata`.

### Phase 1: Ingestion

```
Read metadata  
  → Read repertoires  
    → Preprocess  
      → Aggregate receptors  
        → Aggregate repertoires
```

1) **Read metadata:**

    This step loads any sample‑ or donor‑level info to your environment using `read_metadata()` function.
  
    This step is optional. You can safely skip it if you don't have per-sample pr per-donor metadata, such as therapy response, HLA, age, etc.

2) **Read repertoires:**

    Parquet/CSV/TSV → DuckDB tables via `read_repertoires()`.

3) **Preprocess:**

    Drop unwanted cols, drop nonproductive sequences, rename to AIRR schema, remove duplicate contigs by passing the preprocessing strategy to `read_repertoires()`.

    Optional yet recommended step, turned on by default.

4) **Aggregate receptors:**

    Collapse by CDR3/V/J (or your schema) by passing a receptor schema to `read_repertoires()`.

5) **Aggregate repertoires:**

    Group per sample/donor/time to define set of receptors or repertoires - either inside `read_repertoires()` if you pass a repertoire schema to it, or separately by calling `agg_repertoires()` function.

    Optional step, you can define repertoires later using more information, e.g., in the single-cell case, first, you import cell cluster information, and second, you define repertoires using the donor + cluster information.

### Phase 2: Analysis

```
Import annotations
  → Aggregate repertoires
    → Filter
      → Compute
        → Annotate
          → Export annotations
```

1) **Import annotations:**

    `annotate_cells` from the single-cell data

2) **Aggregate repertoires:**

    Optionally aggregate to repertoires, potentially using the newly annotate data.

3) **Filter:**

    `filter_immundata` gets you identifiers of interest and their corresponding receptor features, potentially using the annotation from the previous step

4) **Compute:**

    On this step, you compute statistics per-repertoire or per-receptor, using input receptor features. There are several scenarios depending on what you try to achieve.

    1) use `immunarch` for the most common analysis functions. The package will automatically annotate both *receptors/cells* (!) and *repertoires* (!!) if it is possible.

    2)  simply mutate on the whole dataset using `dplyr` syntax, like compute the number of cells or whatever

    3) more complex compute that requires a function to apply to values and is probably not supported by `duckplyr`. See the [🧠 Advanced Topics](#-advanced-topics) for more details.

5)  **Annotate:**

    `annotate_immundata` joins the computed values back to the initial dataset using the identifiers. If you already have identifiers, you can simply use `annotate_cells` or `annotate_receptors`.

6)  **Export:**

    `write_annotations` optionally, writes the annotated data back to the cell-level dataset (Seurat / AnnData) for the subsequent analysis. Additionally, you could write the immundata itself to disk if needed.

---

## 💾 Ingestion

### Load AIRR data into `immundata`

**`immundata`** provides a flexible system for loading immune receptor repertoire files from different sources -- CSV, TSV and Parquet files, possibly gzipped, with some optionality. The main function for this is `read_repertoires()`. Below are four ways to pass your file paths.

  1. **Pass a single file name:**
  
      If you just have **one** AIRR file:

      ```r
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

      The `schema` argument tells `immundata` which columns define the unique receptor signature.
      
      By default, `read_repertoires()` writes Parquet files into a directory named `immundata-my_airr_file` and then calls `read_immundata()` on it. Consider passing `output_folder` if you want to specify the output path.

  2. **Pass a vector of file names:**
  
      For **multiple** files in a vector:

      ```r
      many_files <- c("sample1.airr.tsv", "sample2.airr.tsv", "sample3.airr.tsv")
      
      my_immdata <- read_repertoires(
        path   = many_files,
        schema = c("V_gene", "J_gene", "CDR3_nt")
      )
      ```

      `immundata` automatically merges them (depending on your chosen schema) and writes the aggregated data into a single directory of Parquet files.

  3. **Pass a glob pattern:**
  
      If your files follow a consistent naming pattern, you can leverage shell globs:

      ```r
      # For example, all AIRR files in the 'samples/' folder
      my_immdata <- read_repertoires(
        path   = "samples/*.airr.tsv",
        schema = c("V_gene", "J_gene", "CDR3_nt")
      )
      ```

      Behind the scenes, `read_repertoires()` expands the glob with `Sys.glob(...)`, merges the data, and produces a single `ImmunData`. Think about it as a huge table instead of smaller multiple repertoire tables.

  4. **Use a metadata file:**
  
      Sometimes you need more control over the data source (e.g. consistent sample naming, extra columns). In that case:

        1.  **Load metadata** with `read_metadata()`.
        
        2.  **Pass** the resulting data frame to `read_repertoires(path = "<metadata>", ..., metadata = md)`. Mind the `"<metadata>"` string we pass to the function. It indicates that we should take file paths from the input metadata table.

      An example code:


      ```r
      # Suppose metadata.tsv has a column 'File' with paths to your AIRR files
      md <- read_metadata("metadata.tsv", filename_col = "File")
      my_immdata <- read_repertoires(
        path     = "<metadata>"
        metadata = md,
        schema   = c("V_gene", "J_gene", "CDR3_nt")
      )
      ```


      This approach **unifies** sample-level metadata (e.g. donor ID, timepoint) with your repertoire data inside a single `ImmunData`.
      
      The more information on how to work with metadata files, please read the next section.

  5. **Convert from `immunarch` lists:**
  
      Pass `immunarch` data lists to `from_immunarch()` to create `ImmunData` objects.
  
      - [ ] ToDo

### Working with metadata table files

 - [ ] `read_metadata`

### Writing data on disk after preprocessing or analysis

 - [ ] TODO `write_immundata`

### Re-aggregating data using receptor and repertoire schemas

 - [ ] TODO
 
### Preprocessing strategies 

-   filtering non productive
-   double contigs
-   double BCR chains
-   locus

---

## 🛠 Analysis

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

### Compute statistics

#### Basic analysis in `immundata` 

Find receptors from several repertoires or groups which have >2 abundance

#### Exporatory and statistical analysis in `immunarch` 

TODO

---

## 🧩 Use Cases

### Bulk -- RepSeq, AIRRSeq

TODO

### Paired-chain -- scVDJseq or other technologies

TODO

### Single-cell -- scRNAseq, scVDJseq, scTCRseq, scBCRseq

-   load annotation data
-   do something
-   write the annotation data back
-   visualize AIRR with annotations data
-   visualize SC with annotation data

For more information see the vignette tutorial: 

- run `vignette("single_cell")` from R, or
- follow the link to read it online: [link](link).

### Spatial -- spatial transcriptomics and cell coordinates

-   load annotation data
-   do something
-   write the annotation data back
-   visualize AIRR with annotations data
-   visualize SC with annotation data

### Annotate immune receptors using external AIRR databases 

TODO

### Immunogenicity -- run external tools such as TCRdist to annotate ImmunData 

TODO

### Hybrid datasets 

#### Multi-locus data

TODO

#### Multiple contigs for TCR

TODO

#### BCR-heavy chains with multiple light chains

TODO

#### Bulk and single-cell data integration

TODO

### Receptor clusters and motifs

TODO

---

## 🧠 Advanced Topics 

### Integrate into your package 

`immundata` is created in mind with the mission of replacing typical data frame-based formats, usually not following the AIRR-C file standard.

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
    -   If you need to reopen the data in a future R session, you don’t have to redo the entire pipeline.
    -   Just call `read_immundata(path_to_immundata_folder)` where the folder contains `receptors.parquet` and `annotations.parquet`.
    -   With this approach, you **never** need to re-parse your raw AIRR files once you’ve generated the Parquet-based `immundata` format.

Why split it up?

-   **Modularity**: If something breaks, you can debug whether it’s in metadata parsing or the actual repertoire table creation.
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

`immundata` is free to use for commercial usage. However, corporate users will not get a prioritized support for `immundata`-related issues, immune repertoire analysis questions or data engineering questions, related to building scalable immune repertoire and other -omics pipelines. The priority of open-source tool `immundata` is open-source science.

If you are looking for prioritized support and setting up your data pipelines, consider contacting Vadim Nazarov for commercial consulting and support options.

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
