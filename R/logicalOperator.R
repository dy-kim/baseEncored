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

#' Order-independent difference of a set of converted numerics.
#'
#' \code{setdiffNumericForcedCommutative} takes the union of values returned by
#' \code{setdiffNumericForced(x, y)} and \code{setdiffNumericForced(y, x)},
#' returning the total set of unlike integers between \code{x} and \code{y}.
#'
#' @param x first set of differences.
#' @param y second set of differences.
#'
#' @seealso
#' \code{\link{setdiffNumericForced}}: converts character inputs of two sets
#' into numeric and finds the set difference. \cr
#' \code{\link{searchMetaData}}
#'
#' @examples
#' # Here, a and b can be thought of as results from setdiffNumericForced(x, y)
#' # setdiffNumericForced(y, x), respectively.
#' a <- c('4', '5')
#' b <- c('1', '6')
#'
#' setdiffNumericForcedCommutative(a, b)
#'
#' @export
setdiffNumericForcedCommutative <- function(x, y) {
  union(setdiffNumericForced(x, y),
        setdiffNumericForced(y, x)) -> result
  return(result)
}

#' Set difference of converted inputs.
#'
#' \code{setdiffNumericForced} converts numeral character inputs of two sets
#' into numeric and finds the difference between the sets.
#'
#' @param x first set of characters.
#' @param y second set of characters. Note that the function is order dependent,
#' and this is addressed in \code{\link{setdiffNumericForcedCommutative}}.
#'
#' @seealso
#' \code{\link{setdiffNumericForcedCommutative}}: takes the union of the
#' commutative \code{setdiffNumericForced}, returning the total set difference. \cr
#' \code{\link{searchMetaData}}
#'
#' @examples
#' first <- c('3', '4', '5')
#' second <- c('1', '3', '6')
#'
#' setdiffNumericForced(first, second)
#' setdiffNumericForced(second, first)
#'
#' @export
setdiffNumericForced <- function(x, y) {
  setdiff(as.numeric(x), as.numeric(y)) -> result
  return(result)
}

#' @export
"%w/o%" <- function(x, y) {
  return(x[!x %in% y])
}
