PowerUsage <- function(obj,
                       time_unit  = getTimeUnitSet(),
                       power_unit = getPowerUnitSet()) {
  if (!is.data.frame(obj)) {
    warning('NOTICE ME: only data.frame is allowed for Power Usage class.')
    return(obj)
  }

  time.unit  <- match.arg(time_unit)
  power.unit <- match.arg(power_unit)

  class(obj)        <- c('data.frame', 'PowerUsage')
  attr(obj, 'unit') <- power.unit
  attr(obj, 'time') <- time.unit

  return(obj)
}

updatePower.PowerUsage <-
  function(obj, power_unit = getPowerUnitSet()) {
    if (!('PowerUsage' %in% class(obj)))
      stop("NOTICE ME: only PowerUsage object is allowed for updating.")

    power.unit <- match.arg(power_unit)

    if (attr(obj, 'unit') != power.unit) {
      obj$usage <- convertPowerUnit(usage = obj$usage,
                                    from  = attr(obj, 'unit'),
                                    to    = power.unit)
      attr(obj, 'unit') <- power_unit
    }
    return(obj)
  }

convertPowerUnit <- function(usage,
                             from = getPowerUnitSet(),
                             to   = getPowerUnitSet()) {
  from <- match.arg(from)
  to   <- match.arg(to)

  return(usage * getRelativeCoeff_mW_Base(from) / getRelativeCoeff_mW_Base(to))
}

getRelativeCoeff_mW_Base <- function(power_unit) {
  switch(
    power_unit,
    'mW'  = 1,
    'W'   = 1000,
    'kW'  = 1000000
  ) %>%
    return()
}

updateTime.PowerUsage <- function(obj, time_unit = c()) {
  # TO DO
}
