#' @export
expandSciNotation <- function(value) {
  options(scipen = max(nchar(value)))
}

#' @export
setTimeDigit <- function(digits) {
  options(digits.secs = digits)
}

#' @export
chkSysName <- function(sys_name) {
  return(Sys.info()['sysname'] == sys_name)
}
