assertR6Gen <- function (
    x,
    classes = NULL,
    ordered = FALSE,
    cloneable = NULL,
    public = NULL,
    private = NULL,
    null.ok = FALSE,
    .var.name = vname(x),
    add = NULL
) {
  assertClass(BackendEngine, "R6")
}
