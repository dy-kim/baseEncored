nor <- function(x, y) {
  stopifnot(is.logical(x) & is.logical(y))
  return(!x & !y)
}

norIsNullIsNa <- function(x) {
  ifelse(!is.null(x), !is.na(x), FALSE) -> result
  return(result)
}

setdiffNumericForcedCommutative <- function(x, y) {
  union(setdiffNumericForced(x, y),
        setdiffNumericForced(y, x)) -> result
  return(result)
}

setdiffNumericForced <- function(x, y) {
  setdiff(as.numeric(x), as.numeric(y)) -> result
  return(result)
}

"%w/o%" <- function(x, y) {
  return(x[!x %in% y])
}
