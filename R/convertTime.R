# TODO: 함수명이 길다. 줄여보자.
# TODO: lubridate 패키지에 구현된 것이 있는지 확인하고 없다면 contribute

#' @export
convertHumanDate2customTZ <- function(date, time_zone) {
  stopifnot(time_zone %in% OlsonNames())
  as.POSIXct(date, origin = TIME_ORIGIN, tz = time_zone) -> result
  return(result)
}

#' @export
convertHumanDate2KST <- function(date_KST) {
  convertHumanDate2customTZ(date = date_KST,
                            time_zone = "Asia/Seoul") -> result
  return(result)
}

#' @export
convertHumanDate2UTC <- function(date_UTC) {
  convertHumanDate2customTZ(date = date_UTC,
                            time_zone = "UTC") -> result
  return(result)
}

#' @export
convertHumanDateKST2timestampUTC <- function(date_KST) {
  convertHumanDate2KST(date_KST) %>%
    as.numeric() -> result
  return(result)
}

#' @export
convertTimestampUTC2HumandateCustomTZ <-
  function(timestamp_ms, time_zone) {
    stopifnot(time_zone %in% OlsonNames())
    convertHumanDate2customTZ(date = as.numeric(timestamp_ms) / 1000,
                              time_zone = time_zone) -> result
    return(result)
  }

#' @export
convertTimestampUTC2HumandateKST <- function(timestamp_ms) {
  convertTimestampUTC2HumandateCustomTZ(timestamp_ms, "Asia/Seoul") -> result
  return(result)
}

#' @export
convertTimezone <- function(dateTimeClass, TZ_to) {
  stopifnot(TZ_to %in% OlsonNames())
  format(x = dateTimeClass, tz = TZ_to) %>%
    convertHumanDate2customTZ(time_zone = TZ_to) -> result
  return(result)
}

#' @export
convertJsonDateEncored <-
  function(date_UTC, target_TZ = TZ_DEFAULT) {
    stopifnot(target_TZ %in% OlsonNames())

    humanDateUtc <-
      as.character(date_UTC) %>%
      stringr::str_replace(pattern = "T", replacement = " ") %>%
      stringr::str_replace(pattern = "Z", replacement = "")
    convertHumanDate2UTC(humanDateUtc) %>%
      convertTimezone(TZ_to = target_TZ) -> result

    return(result)
  }

#' @export
timeSequenceEncored <- function(start_date,
                                end_date,
                                time_unit,
                                num_seq = NULL,
                                tz = TZ_DEFAULT) {
  stopifnot(tz %in% OlsonNames())

  time_unit %<>% match.arg(choices = getTimeUnitSet())
  start <- convertHumanDate2customTZ(start_date, tz)
  end <- convertHumanDate2customTZ(end_date, tz)

  ifelse(
    test = is.null(num_seq),
    yes  = convertTimeUnitAsSec(time_unit),
    no   = as.numeric(difftime(end, start, tz, units = "sec") / num_seq)
  ) %>%
    seq(from = start, to = end, by = .) -> result

  return(result)
}

#' @export
countTimeSlot <- function(start_date, end_date, time_unit) {
  time_unit %<>% match.arg(choices = getTimeUnitSet())

  timeSequenceEncored(start_date, end_date, time_unit) %>%
    length() -> result

  return(result)
}

#' @export
convertTimeUnitAsSec <- function(time_unit) {
  time_unit %<>% match.arg(choices = getTimeUnitSet())

  switch(
    time_unit,
    "15min"   = 60 * 15,
    "hourly"  = 60 * 60,
    "daily"   = 60 * 60 * 24,
    "monthly" = 60 * 60 * 24 * 30
  ) -> result

  return(result)
}

#' @export
roundTime <- function(time_obj, time_unit, round_func) {
  timeUnitAsSec <- convertTimeUnitAsSec(time_unit)
  (round_func(as.numeric(time_obj) / timeUnitAsSec) * timeUnitAsSec * 1000) %>%
    convertTimestampUTC2HumandateKST() -> result
  return(result)
}

#' @export
getQueryTimestamp <- function(date_KST) {
  (convertHumanDateKST2timestampUTC(date_KST) * 1000) %>%
    formatNonSci() -> result
  return(result)
}

#' @export
getQueryStartTimestamp <- function(start_date) {
  paste0("start=", getQueryTimestamp(start_date)) -> result
  return(result)
}

#' @export
getQueryEndTimestamp <- function(end_date) {
  paste0("end=", getQueryTimestamp(end_date)) -> result
  return(result)
}

#' @export
convertTimeUnit2PeriodClass <- function(time_unit) {
  time_unit %<>% match.arg(choices = getTimeUnitSet())

  switch(
    time_unit,
    "15min"  = lubridate::minutes(15),
    "hourly" = lubridate::hours(1),
    "daily"  = lubridate::days(1)
  ) -> result

  return(result)
}

#' @export
sysTimeEncored <- function() {
  Sys.time() %>%
    format(format = "%Y-%m-%d %H:%M:%S") -> result
  return(result)
}

#' @export
isTimestampUnitSecond <- function(timestamp) {
  # '2286-11-21 02:46:39'까지가 nchar(timestamp) == 10
  timestamp <- assureNumeric(timestamp)

  if (is.null(timestamp)) {
    warning("Please check that the input for timestamp is proper type. Return NULL.")
    return(NULL)
  }

  dateMax <- "9999-12-31"
  timsestampSecondUnitMax <-
    dateMax %>%
    convertHumanDateKST2timestampUTC() %>%
    convertTimestampUTC2HumandateKST()

  isUnitSecond <-
    convertTimestampUTC2HumandateKST(max(timestamp)) < timsestampSecondUnitMax

  return(isUnitSecond)
}
