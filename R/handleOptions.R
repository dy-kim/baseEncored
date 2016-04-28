expandSciNotation <- function(value) {
  options(scipen = max(nchar(value)))
}

setTimeDigit <- function(digits) {
  options(digits.secs = digits)
}

chkSysName <- function(sys_name) {
  return(Sys.info()['sysname'] == sys_name)
}
