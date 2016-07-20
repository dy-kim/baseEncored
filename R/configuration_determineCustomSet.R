#' @export
getTimeUnitSet <-
  function(data_src = c("actual", "nilm", "EnerTalkBuilding")) {
    data.src <- match.arg(data_src)

    if (data.src == "nilm")
      return(c("hourly", "daily"))

    if (data.src == "EnerTalkBuilding")
      return(c("15min", "hourly", "daily", "monthly"))

    return(c("15min", "hourly", "daily", "monthly"))
  }

#' @export
getPowerUnitSet <- function() {
  return(c("mW", "W", "kW"))
}

#' @export
getDbGroup <- function() {
  return(c("getit", "production", "setup", "data_cache"))
}

#' @export
getDbConfigDir <- function() {
  # nolint start
  return("/etc/.my.cnf")
  # nolint end
}

#' @export
getConvertibleBase <- function() {
  return(c("2", "10", "16"))
}
