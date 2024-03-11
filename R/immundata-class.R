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
'



#' @importFrom R6 R6Class
ImmunData <- R6Class(
  "ImmunData",
  private = list(
    dataset = NULL,
    metadata = NULL
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
    summarize = function(...) {
      ImmunData$new(
        private$dataset %>% summarize(...),
        private$metadata
      )
    },
    collect = function() {
      private$dataset %>% collect()
    },
    as = function(.rep = "Sample", .cl = "aa+v") {
      # TODO: store default .by for single-cell in future
      # TODO: Sort by proportion
      for (i in 1:length(.by)) {
        col_name = .by[i]
        if (!(col_name %in% colnames(private$dataset))) {
          .by[i] = paste0("Metadata.", col_name)
        }
      }

      ImmunData$new(
        private$dataset %>% group_by(Metadata.Sample),
        private$metadata
      )
    },
    to_list = function() {
      # TODO: group_split and/or check
      list(data = private$dataset %>% collect(),
           meta = private$metadata)
    }
  )
)


select.ImmunData <- function(.immdata, ...) {
  .immdata$select(...)
}

collect.ImmunData <- function(.immdata, ...) {
  .immdata$collect(...)
}


imd <- ImmunData$new(immdata)
imd2 <- imd %>% select(CDR3.aa)
imd2 %>% collect()
