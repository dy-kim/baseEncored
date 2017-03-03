#' @importFrom future future %plan% multiprocess
saveRdsAsnyc <- function(...) {
  withAsnyc(saveRDS, ...)
}

readRdsAsync <- function(...) {
  withAsnyc(readRDS, ...)
}

withAsnyc <- function(f, ...) {
  # nolint start
  future({
    f(...)
  }) %plan% multiprocess
  # nolint end
}
