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


# https://dplyr.tidyverse.org/reference/index.html
# https://dplyr.tidyverse.org/reference/dplyr_extending.html
# https://github.com/tidyverse/dplyr/blob/b359331a448a693546d77245b0de4d405bab3886/NAMESPACE#L58
# https://cran.r-project.org/web/packages/checkmate/index.html


library(R6)

RepertoireModel <- R6Class(
  "RepertoireModel",
  public = list(
    by = "",
    names = "",
    initialize = function(.by, .names) {
      ###
      # Argument type checks
      ###
      assert_character(.by, min.chars = 1, min.len = 1)
      assert(
        check_character(.names, min.chars = 1, .min.len = 1),
        check_function(.names)
      )

      ###
      # Body
      ###
      self$by <- .by
      self$names <- .names # TODO: when do we call the function to generate repertoire names?
    },
    print = function() {

    }
  )
)

ClonotypeModel <- R6Class(
  "ClonotypeModel",
  public = list(
    initialize = function(.code = "", .chain = "", .sequence = "", .gene = "") {
      assert_clonotype_info <- function(.chain, .sequence, .gene) {
        assert(
          assert_character(.chain, pattern = ""),
          assert_character(.sequence, pattern = ""),
          assert_character(.gene, pattern = ""),
          combine = "and"
        )
      }

      ###
      # Argument type checks
      ###
      assert(
        assert_character(.code, .min.len = 1),
        assert_clonotype_info(.chain, .sequence, .gene)
      )

      if (nchar(.code)) {
        .code <- stringr::str_split(input, "\\+") |> unlist() # Yes, I do like pipes, they improve the readability.
        # .chain
        # .sequence
        # .gene
        assert_clonotype_info(.chain, .sequence, .gene)
      }

      ###
      # Body
      ###
    }
  )
)

#' @importFrom R6 R6Class
#' @export
ImmunData <- R6Class(
  "ImmunData",
  private = list(
    dataset = NULL,
    metadata = NULL,
    repertoire_model = NULL,
    clonotype_model = NULL,
    repertoire_names = NULL
    # from_list
    # from_table
    # from_text_file
    # from_database
  ),
  public = list(
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      # if (inherits(dataset, "list")) {
      #   # If dataset is a list with "data" and "meta" from immunarch
      #   if (all(c("data", "meta") %in% names(dataset))) {
      #     metadata <- dataset$meta
      #     dataset <- dataset$data
      #   }
      #   # TODO: if no metadata, and metadata is NULL
      #
      #   # If dataset is a list with immune repertoires, and metadata is passed separately
      #   private$dataset <- lapply(names(dataset), function(df_name) {
      #     metadata_row <- metadata %>%
      #       filter(Sample == df_name)
      #     metadata_row <- rename_all(
      #       metadata_row,
      #       function(x) paste("Metadata", x, sep = ".")
      #     )
      #
      #     metadata_row <- metadata_row %>% slice(rep(1:n(), each = nrow(dataset[[df_name]])))
      #
      #     bind_cols(dataset[[df_name]], metadata_row)
      #   })
      #
      #   private$dataset <- do.call(rbind, private$dataset)
      #
      #   private$metadata <- metadata
      # } else {
      #   # If dataset is a single immune repertoire file
      #   private$dataset <- dataset
      #   private$metadata <- metadata
      # }
    },
    to_list = function() {
      dataset <- private$dataset %>% group_split()
      names(dataset) <- private$repertoire_names
      metadata <- NULL
      list(data = dataset, meta = metadata)
    },
    filter = function(...) {
      private$dataset %>% filter(...)
    },
    select = function(...) {
      private$dataset %>% select(...)
    },
    group_by = function(...) {
      private$dataset %>% group_by(...)
    },
    summarise = function(...) {
      private$dataset %>% summarise(...)
    },
    collect = function() {
      private$dataset %>% collect()
    },
    `[[` = function(name) {
      if (!name %in% names(private$dataset)) {
        stop("Repertoire '", name, "' not found in the data.", call. = FALSE)
      }
      return(self$data[[name]])
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

#' @importFrom tibble as_tibble
#' @export
DataFrameImmunData <- R6Class(
  "DataFrameImmunData",
  inherit = ImmunData,
  private = list(

  ),
  public = list(
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      private$dataset <- as_tibble(.dataset)
      private$metadata <- as_tibble(.metadata)
    }
  )
)


#' @importFrom tidytable as_tidytable
#' @export
DataTableImmunData <- R6Class(
  "DataTableImmunData",
  inherit = ImmunData,
  private = list(

  ),
  public = list(
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      private$dataset <- as_tidytable(.dataset)
      private$metadata <- as_tibble(.metadata)
    }
  )
)
