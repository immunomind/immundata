# https://dplyr.tidyverse.org/reference/index.html
# https://dplyr.tidyverse.org/reference/dplyr_extending.html
# https://github.com/tidyverse/dplyr/blob/b359331a448a693546d77245b0de4d405bab3886/NAMESPACE#L58
# https://cran.r-project.org/web/packages/checkmate/index.html
#' @importFrom R6 R6Class
#' @keywords internal
DataManipulationInterface <- R6Class(
  "DataManipulationInterface",
  public = list(
    initialize = function(data, ...) {

    },

    collect = function(...) {
      stop(MESSAGES[["NotImpl"]])
    },

    select = function(...) {
      stop(MESSAGES[["NotImpl"]])
    },

    filter = function(...) {
      stop(MESSAGES[["NotImpl"]])
    },

    group_by = function(...) {
      stop(MESSAGES[["NotImpl"]])
    },

    summarise = function(...) {
      stop(MESSAGES[["NotImpl"]])
    },

    summarize = function(...) {
      stop(MESSAGES[["NotImpl"]])
    },

    show = function() {
      stop(MESSAGES[["NotImpl"]])
    }
  )
)
