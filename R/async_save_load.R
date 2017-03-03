#' @importFrom future future %plan% multiprocess
saveRdsAsnyc <- function(...) {
  withAsnyc(saveRDS, ...)
}

readRdsAsync <- function(...) {
  withAsnyc(readRDS, ...)
}

withAsnyc <- function(f, ...) {
  future({
    f(...)
  }) %plan% multiprocess
}
