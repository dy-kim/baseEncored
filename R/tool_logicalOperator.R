nor <- function(x, y) {
  stopifnot(is.logical(x) & is.logical(y))
  return(!x & !y)
}

norIsNullIsNa <- function(x) {
  ifelse(!is.null(x), !is.na(x), FALSE) %>%
    return()
}

setdiffNumericForcedCommutative <- function(x, y) {
  union(setdiffNumericForced(x, y),
        setdiffNumericForced(y, x)) %>%
    return()
}

setdiffNumericForced <- function(x, y) {
  setdiff(as.numeric(x), as.numeric(y)) %>%
    return()
}

"%w/o%" <- function(x, y) x[!x %in% y]
