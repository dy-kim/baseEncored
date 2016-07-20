#' @export
PowerUsage <- function(obj, time_unit, power_unit) {
  time_unit  %<>% match.arg(choices = getTimeUnitSet())
  power_unit %<>% match.arg(choices = getPowerUnitSet())
  if (!is.data.frame(obj)) {
    warning("NOTICE ME: only data.frame is allowed for Power Usage class.")
    return(obj)
  }
  class(obj) <- c("data.frame", "PowerUsage")
  attr(obj, "unit") <- power_unit
  attr(obj, "time") <- time_unit
  return(obj)
}

#' @export
updatePower.PowerUsage <- function(obj, power_unit) {
  power_unit %<>% match.arg(choices = getPowerUnitSet())
  if (!("PowerUsage" %in% class(obj)))
    stop("NOTICE ME: only PowerUsage object is allowed for updating.")
  if (attr(obj, "unit") != power_unit) {
    obj$usage <- convertPowerUnit(usage = obj$usage,
                                  from = attr(obj, "unit"),
                                  to = power_unit)
    attr(obj, "unit") <- power_unit
  }
  return(obj)
}

#' @export
convertPowerUnit <- function(usage, from, to) {
  from %<>% match.arg(choices = getPowerUnitSet())
  to   %<>% match.arg(choices = getPowerUnitSet())
  return(usage * getRelativeCoeff_mW_Base(from) / getRelativeCoeff_mW_Base(to))
}

#' @export
getRelativeCoeff_mW_Base <- function(power_unit) {
  power_unit %<>% match.arg(choices = getPowerUnitSet())
  switch(power_unit,
         "mW" = 1,
         "W" = 1000,
         "kW" = 1000000) -> result
  return(result)
}

updateTime.PowerUsage <- function(obj, time_unit = c()) {
  # TO DO
}
