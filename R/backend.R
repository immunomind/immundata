Backend <- R6Class(
  "Backend",
  private = list(
    data_source = NULL,
    operations = NULL,

    check_data_source = function(.data_source) {
      stop(MESSAGES$NotImpl)
    }
  ),
  public = list(
    initialize = function(.data_source, .operations) {
      private$check_data_source(.data_source)
      check_list(.operations)

      private$data_source <- .data_source
      private$operations <- .operations
    },

    ####
    ## Row operations
    ####

    arrange = function(...) {
      stop(MESSAGES$NotImpl)
    },
    filter = function(...) {
      private$data_source |> filter(...)
    },
    slice = function(...) {
      stop(MESSAGES$NotImpl)
    },
    slice_head = function(...) {
      stop(MESSAGES$NotImpl)
    },
    slice_tail = function(...) {
      stop(MESSAGES$NotImpl)
    },

    ####
    ## Column operations
    ####

    mutate = function(...) {
      private$data_source |> mutate(...)
    },
    select = function(...) {
      private$data_source |> select(...)
    },

    ####
    ## Group operations
    ####

    count = function(...) {
      stop(MESSAGES$NotImpl)
    },
    group_by = function(...) {
      # .add = TRUE - ???
      private$data_source |> group_by(...)
    },
    summarise = function(...) {
      private$data_source |> summarise(...)
    },

    ####
    ## Data operations
    ####

    compute = function(...) {
      stop(MESSAGES$NotImpl)
    },
    collect = function(...) {
      private$data_source |> group_by(...)
    }
  )
)
