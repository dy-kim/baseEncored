#' @export
expandSciNotation <- function(value) {
  options(scipen = max(nchar(value)))
}

#' @export
expandDigit <- function(value) {
  digit.to <- nchar(value)

  if (grepl("\\.", value))
    digit.to <- digit.to - 1

  if (digit.to > 22)
    digit.to <- 22

  options(digits = digit.to)
}

#' @export
setDigit <- function(digits) {
  options(digits = digits)
}

#' @export
setTimeDigit <- function(digits) {
  options(digits.secs = digits)
}

#' @export
chkSysName <- function(sys_name) {
  return(Sys.info()["sysname"] == sys_name)
}
