# https://dplyr.tidyverse.org/reference/index.html
# https://dplyr.tidyverse.org/reference/dplyr_extending.html
# https://github.com/tidyverse/dplyr/blob/b359331a448a693546d77245b0de4d405bab3886/NAMESPACE#L58
# https://cran.r-project.org/web/packages/checkmate/index.html

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
        check_character(.names, min.chars = 1, min.len = 1),
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
          check_character(.chain, pattern = ""),
          check_character(.sequence, pattern = ""),
          check_character(.gene, pattern = ""),
          combine = "and"
        )
      }

      ###
      # Argument type checks
      ###
      assert(
        check_character(.code),
        check_clonotype_info(.chain, .sequence, .gene)
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
    repertoire_names = NULL,
    # from_data_frame
    # from_list
    # from_text_file
    # from_folder
    # from_database
    # from_immundata_file
    # from_immundata_database

    create_instance = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      stop(MESSAGES$NotImpl)
    }
  ),
  public = list(

    ####
    ## Basic interface
    ####
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      private$dataset <- .dataset
      private$metadata <- .metadata
      private$repertoire_model <- .repertoire_model
      private$clonotype_model <- .clonotype_model
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
    finalize = function() {

    },
    to_list = function() {
      dataset <- private$dataset %>% group_split()
      names(dataset) <- private$repertoire_names
      metadata <- NULL
      list(data = dataset, meta = metadata)
    },
    print = function() {
      class(self)
    },

    ####
    ## Row operations
    ####

    arrange = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% arrange(...),
        .metadata = private$metadata
      )
    },
    filter = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% filter(...),
        .metadata = private$metadata
      )
    },
    slice = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% slice(...),
        .metadata = private$metadata
      )
    },
    slice_head = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% slice_head(...),
        .metadata = private$metadata
      )
    },
    slice_tail = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% slice_tail(...),
        .metadata = private$metadata
      )
    },

    ####
    ## Column operations
    ####

    mutate = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% mutate(...),
        .metadata = private$metadata
      )
    },
    select = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% select(...),
        .metadata = private$metadata
      )
    },

    ####
    ## Group operations
    ####

    count = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% count(...),
        .metadata = private$metadata
      )
    },
    group_by = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% group_by(...),
        .metadata = private$metadata
      )
    },
    summarise = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% summarise(...),
        .metadata = private$metadata
      )
    },

    ####
    ## Data operations
    ####

    compute = function(...) {
      private$create_instance(
        .dataset = private$dataset %>% compute(...),
        .metadata = private$metadata
      )
    },
    collect = function(...) {
      private$dataset %>% collect(...)
    },
    `[[` = function(.sample) {
      check_character(.sample)

      # Not sample but columns by which was grouped
      # OR the sample or ID
      private$dataset |> filter(Sample == .sample)
    },
    data = function() {
      private$dataset
    }
  )
)

####
## Row operations
####

#' @exportS3Method dplyr::arrange
arrange.ImmunData <- function(.immdata, ...) {
  .immdata$arrange(...)
}

#' @exportS3Method dplyr::filter
filter.ImmunData <- function(.immdata, ...) {
  .immdata$filter(...)
}

#' @exportS3Method dplyr::slice
slice.ImmunData <- function(.immdata, ...) {
  .immdata$slice(...)
}

#' @exportS3Method dplyr::slice_head
slice_head.ImmunData <- function(.immdata, ...) {
  .immdata$slice_head(...)
}

#' @exportS3Method dplyr::slice_tail
slice_tail.ImmunData <- function(.immdata, ...) {
  .immdata$slice_tail(...)
}

####
## Column operations
####

#' @exportS3Method dplyr::mutate
mutate.ImmunData <- function(.immdata, ...) {
  .immdata$mutate(...)
}

#' @exportS3Method dplyr::select
select.ImmunData <- function(.immdata, ...) {
  .immdata$select(...)
}

####
## Group operations
####

#' @exportS3Method dplyr::count
count.ImmunData <- function(.immdata, ...) {
  .immdata$count(...)
}

#' @exportS3Method dplyr::group_by
group_by.ImmunData <- function(.immdata, ...) {
  .immdata$group_by(...)
}

#' @exportS3Method dplyr::summarise
summarise.ImmunData <- function(.immdata, ...) {
  .immdata$summarise(...)
}

#' @exportS3Method dplyr::summarize
summarize.ImmunData <- function(.immdata, ...) {
  .immdata$summarise(...)
}

####
## Data operations
####

#' @exportS3Method dplyr::compute
compute.ImmunData <- function(.immdata, ...) {
  .immdata$compute(...)
}

#' @exportS3Method dplyr::collect
collect.ImmunData <- function(.immdata, ...) {
  .immdata$collect(...)
}


#' @importFrom tibble as_tibble
#' @export
DataFrameImmunData <- R6Class(
  "DataFrameImmunData",
  inherit = ImmunData,
  private = list(
    create_instance = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      DataFrameImmunData$new(.dataset = .dataset, .metadata = .metadata)
    }
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
    create_instance = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      DataTableImmunData$new(.dataset = .dataset, .metadata = .metadata)
    }
  ),
  public = list(
    initialize = function(.dataset, .metadata, .repertoire_model = NULL, .clonotype_model = NULL) {
      private$dataset <- as_tidytable(.dataset)
      private$metadata <- as_tibble(.metadata)
    }
  )
)
