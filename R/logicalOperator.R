#' @export
nor <- function(x, y) {
  stopifnot(is.logical(x) & is.logical(y))
  return(!x & !y)
}

#' @export
norIsNullIsNa <- function(x) {
  ifelse(!is.null(x), !is.na(x), FALSE) -> result
  return(result)
}

#' @export
setdiffNumericForcedCommutative <- function(x, y) {
  union(setdiffNumericForced(x, y),
        setdiffNumericForced(y, x)) -> result
  return(result)
}

#' @export
setdiffNumericForced <- function(x, y) {
  setdiff(as.numeric(x), as.numeric(y)) -> result
  return(result)
}

#' @export
"%w/o%" <- function(x, y) {
  return(x[!x %in% y])
}
