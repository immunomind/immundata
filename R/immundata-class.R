'
# old ways:
singledata %>%
  clonotypeModel("aa+v") %>%
  repertoireModel(group_by = c("Sample")) %>%
  immdata %>%
  repOverlap()

# new ways:
singledata %>%
  repertoireModel(group_by = c("Sample")) %>%
  immdata %>%
  asClonotype("aa+v") %>%
  repOverlap()

# new ways II:
immdata = singledata %>%
  repertoireModel(group_by = c("Sample"))

immdata@asClonotype("aa+v") %>%
  repOverlap()

immdata@as("aa+v") %>%
  repOverlap()


Option 1 "magic class": put "as" into ImmunData$new()
> imd <- ImmunData$new(immdata, .rep="Sample", .cl="ab+nt+vj")

Option 2 "no magic": "as" separately
> imd <- ImmunData$new(immdata)
> imd_sample_cln <- ImmunData$as(.rep="Sample", .cl="ab+nt+vj")

Option 3 "true magic": full flexibility
> imd <- ImmunData$new(immdata, .rep="Sample", .cl="ab+nt+vj")
> new_imd <- imd$as(.rep=c("Sample", "Cluster"), .cl="ab+aa+vj")

Problems for Option 3:
> imd <- ImmunData$new(immdata, .rep="Sample", .cl="ab+nt+vj")
> imd2 <- imd %>% select(V.name)  # imd2$dataset == imd$dataset %>% dplyr::select(V.name)
> imd3 <- imd2$as(.rep=c("Sample", "Cluster"), .cl="ab+aa+vj")  # should return the error

ImmunData states:
1) no repertoire/clonotypes models -> no group by "sample" and "aa+vj"
2) yes repertoire/clonotype models -> there was a group by "sample" and "aa+vj"
3) after applying at least one dplyr function

gordon <- ImmunData%new(load_gordon_data())  # raw data is still here
gordon_beta <- gordon$as("Sample", "b+nt+vj")  # grouped by Sample and selected+grouped by beta+nuc+V+J
gordon_beta %>%
  filter(Metadata.Response %in% c("CR", "PR")) %>%
  geneUsage()

>>>loader = ImmuneRepertoireLoader$new("/path/to/airr/data")
>>>loader$detectedAIRRdatasets()
"datasetMiXCR1.txt"
"datasetMiXCR2.txt"
"datasetMiXCR3.txt"
"datasetMiXCR5.txt"
>>>immdata = loader$getRepertoire(.ds = "datasetMiXCR1.txt", .rep="Sample", .cl="b+nt+vj")
'

# Engine for data loader that is also executes finalize because of e.g., database connections

library(R6)

#' @importFrom R6 R6Class
ImmunDataLoader <- R6Class(
  "ImmunDataLoader",
  private = list(
    source_path = "",
    engine = NULL,
    scan_files = function(.format = NA) {
      # Get all files recursively
    }
  ),
  public = list(
    initialize = function(.path, .engine = "df", .format = NA) {

    },
    finalize = function() {

    },
    list = function() {

    },
    load = function() {

    },
    load_all = function() {

    }
  )
)

#' @importFrom R6 R6Class
ImmunData <- R6Class(
  "ImmunData",
  private = list(
    dataset = NULL,
    metadata = NULL,
    repertoire_model = c(),
    clonotype_model = c(),
    repertoire_names = c()
  ),
  public = list(
    initialize = function(dataset, metadata = NULL) {
      if (inherits(dataset, "list")) {
        # If dataset is a list with "data" and "meta" from immunarch
        if (all(c("data", "meta") %in% names(dataset))) {
          metadata <- dataset$meta
          dataset <- dataset$data
        }
        # TODO: if no metadata, and metadata is NULL

        # If dataset is a list with immune repertoires, and metadata is passed separately
        private$dataset <- lapply(names(dataset), function(df_name) {
          metadata_row <- metadata %>%
            filter(Sample == df_name)
          metadata_row <- rename_all(
            metadata_row,
            function(x) paste("Metadata", x, sep = ".")
          )

          metadata_row <- metadata_row %>% slice(rep(1:n(), each = nrow(dataset[[df_name]])))

          bind_cols(dataset[[df_name]], metadata_row)
        })

        private$dataset <- do.call(rbind, private$dataset)

        private$metadata <- metadata
      } else {
        # If dataset is a single immune repertoire file
        private$dataset <- dataset
        private$metadata <- metadata
      }
    },
    as = function(.rep = "Sample", .cl = "ab+cdr3aa+vj") {
      # TODO: store default .rep for single-cell in future
      # TODO: Sort by proportion
      # TODO: two types of ImmunData - raw without casting "as', and processed after
      # casting "as".
      # TODO: check for consistency of columns so no columns "Sample" are presented in
      # addition to Metadata's sample. Or maybe just store metadata columns in a separate
      # vector, and that's it. Also provide additional functions like "strip all metadata columns"
      # TODO: if the ImmunData has already been grouped by repertoire, then .rep should behave like
      # .cl for user's convenience. E.g., immdata$as("Sample")$as("aa+v") is the same as immdata$as("Sample", "aa+v")
      for (i in 1:length(.rep)) {
        col_name <- .rep[i]
        if (!(col_name %in% colnames(private$dataset))) {
          .rep[i] <- paste0("Metadata.", col_name)
        }
      }

      ImmunData$new(
        private$dataset %>% group_by(Metadata.Sample),
        private$metadata
      )
    },
    to_list = function() {
      dataset <- private$dataset %>% group_split()
      names(dataset) <- private$repertoire_names
      metadata <- NULL
      list(data = dataset, meta = metadata)
    },
    filter = function(...) {
      ImmunData$new(
        private$dataset %>% filter(...),
        private$metadata
      )
    },
    select = function(...) {
      ImmunData$new(
        private$dataset %>% select(...),
        private$metadata
      )
    },
    group_by = function(...) {
      ImmunData$new(
        private$dataset %>% group_by(...),
        private$metadata
      )
    },
    summarise = function(...) {
      ImmunData$new(
        private$dataset %>% summarise(...),
        private$metadata
      )
    },
    collect = function() {
      private$dataset %>% collect()
    }
  )
)


select.ImmunData <- function(.immdata, ...) {
  .immdata$select(...)
}

collect.ImmunData <- function(.immdata, ...) {
  .immdata$collect(...)
}

summarise.ImmunData <- function(.immdata, ...) {
  .immdata$summarise(...)
}

summarize.ImmunData <- function(.immdata, ...) {
  .immdata$summarise(...)
}

group_by.ImmunData <- function(.immdata, ...) {
  .immdata$group_by(...)
}
