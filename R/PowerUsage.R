#' @export
PowerUsage <- function(obj, time_unit, power_unit) {
  time_unit  %<>% match.arg(choices = getTimeUnitSet())
  power_unit %<>% match.arg(choices = getPowerUnitSet())

  if (!is.data.frame(obj)) {
    warning("NOTICE ME: only data.frame is allowed for Power Usage class.")
    return(obj)
  }

  class(obj) <- c("data.frame", "PowerUsage")
  obj %<>% setPowerUnit.PowerUsage(power_unit)
  obj %<>% setTimeUnit.PowerUsage(time_unit)

  return(obj)
}

#' @export
updatePower.PowerUsage <- function(obj, power_unit) {
  warning("Replace this function to 'updatePowerUnit.PowerUsage'!")
  return(updatePowerUnit.PowerUsage(obj, power_unit))
}

#' @export
updatePowerUnit.PowerUsage <- function(obj, powerUnit) {
  powerUnit %<>% match.arg(choices = getPowerUnitSet())

  if (!("PowerUsage" %in% class(obj)))
    stop("NOTICE ME: only PowerUsage object is allowed for updating.")

  timeUnitOriginal <- getTimeUnit.PowerUsage(obj)
  powerUnitOriginal <- getPowerUnit.PowerUsage(obj)
  if (powerUnitOriginal != powerUnit) {
    obj$usage <- convertPowerUnit(usage = obj$usage,
                                  from = powerUnitOriginal,
                                  to = powerUnit)
    obj %<>% setPowerUnit.PowerUsage(powerUnit)
    obj %<>% setTimeUnit.PowerUsage(timeUnitOriginal)
  }
  return(obj)
}

#' @export
updateTimeUnit.PowerUsage <- function(obj, timeUnit) {
  timeUnit %<>% match.arg(choices = getTimeUnitSet())

  if (!("PowerUsage" %in% class(obj)))
    stop("NOTICE ME: only PowerUsage object is allowed for updating.")

  if (!lubridate::is.POSIXt(obj$date))
    stop("Column 'date' must be a POSIXt class!")

  stopifnot(isTimeUnitUpdatable(obj, timeUnit))

  timeUnitOriginal <- getTimeUnit.PowerUsage(obj)
  powerUnitOriginal <- getPowerUnit.PowerUsage(obj)
  if (timeUnitOriginal != timeUnit) {
    obj %<>% convertTimeUnit(to = timeUnit)
    obj %<>% setPowerUnit.PowerUsage(powerUnitOriginal)
    obj %<>% setTimeUnit.PowerUsage(timeUnit)
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

isTimeUnitUpdatable <- function(obj, timeUnit) {
  timeUnitOriginal <- getTimeUnit.PowerUsage(obj)
  timeUnitUpdated <- getConvertibleTimeUnit(timeUnitOriginal)
  return(timeUnit %in% timeUnitUpdated)
}

getConvertibleTimeUnit <- function(timeUnit) {
  timeUnit  %<>% match.arg(choices = getTimeUnitSet())

  convertibleTimeUnit <-
    switch(
      timeUnit,
      "15min" = c("hourly", "daily", "monthly"),
      "hourly" = c("daily", "monthly"),
      "daily" = c("monthly")
    )
  return(convertibleTimeUnit)
}

convertTimeUnit <- function(obj, to) {
  obj %<>% updateDateTimeColumn(to)

  objDt <- data.table::as.data.table(obj)
  keyToGroup <- getGroupKeyToConvertTimeUnit(obj)
  # nolint start
  objDt <- objDt[,
                 .(usage = sum(usage), count = .N),
                 by = keyToGroup]
  # TODO: Make if-clause as a function
  if (to == "hourly") {
    objDt <- objDt[count == NUM_OF_15MIN_IN_AN_HOUR]
  }
  objDt <- objDt[, count := NULL]
  # nolint end

  return(objDt)
}

updateDateTimeColumn <- function(obj, to) {
  timeUnitLubridate <-
    switch(to,
           hourly = "hour",
           daily = "day",
           monthly = "month")

  obj$date %<>% lubridate::floor_date(timeUnitLubridate)
  return(obj)
}

getGroupKeyToConvertTimeUnit <- function(obj) {
  return(names(obj) %w/o% c("usage"))
}

getTimeUnit.PowerUsage <- function(obj) {
  attr(obj, "time")
}

getPowerUnit.PowerUsage <- function(obj) {
  return(attr(obj, "unit"))
}

setTimeUnit.PowerUsage <- function(obj, x) {
  attr(obj, "time") <- x
  return(obj)
}

setPowerUnit.PowerUsage <- function(obj, x) {
  attr(obj, "unit") <- x
  return(obj)
}
