convertHumanDate2customTZ <- function(date, time_zone) {
  stopifnot(time_zone %in% OlsonNames())
  as.POSIXct(date, origin = TIME_ORIGIN, tz = time_zone) %>%
    return()
}

convertHumanDate2KST <- function(date_KST) {
  convertHumanDate2customTZ(date = date_KST,
                            time_zone = 'Asia/Seoul') %>%
    return()
}

convertHumanDate2UTC <- function(date_UTC) {
  convertHumanDate2customTZ(date = date_UTC,
                            time_zone = 'UTC') %>%
    return()
}

convertHumanDateKST2timestampUTC <- function(date_KST) {
  convertHumanDate2KST(date_KST) %>%
    as.numeric() %>%
    floor() %>%
    return()
}

convertTimestampUTC2HumandateCustomTZ <-
  function(timestamp_ms, time_zone) {
    stopifnot(time_zone %in% OlsonNames())
    convertHumanDate2customTZ(date = as.numeric(timestamp_ms) / 1000,
                              time_zone = time_zone) %>%
      return()
  }

convertTimestampUTC2HumandateKST <- function(timestamp_ms) {
  convertTimestampUTC2HumandateCustomTZ(timestamp_ms, 'Asia/Seoul') %>%
    return()
}

convertTimezone <- function(dateTimeClass, TZ_to) {
  stopifnot(TZ_to %in% OlsonNames())

  format(x = dateTimeClass, tz = TZ_to) %>%
    convertHumanDate2customTZ(time_zone = TZ_to) %>%
    return()
}

convertJsonDateEncored <-
  function(date_UTC, target_TZ = TZ_DEFAULT) {
    stopifnot(target_TZ %in% OlsonNames())

    human.date.utc <-
      as.character(date_UTC) %>%
      str_replace(pattern = 'T', replacement = ' ') %>%
      str_replace(pattern = 'Z', replacement = '')

    convertHumanDate2UTC(human.date.utc) %>%
      convertTimezone(TZ_to = target_TZ) %>%
      return()
  }

timeSequenceEncored <-
  function(start_date,
           end_date,
           time_unit = getTimeUnitSet(),
           num_seq = NULL,
           tz = TZ_DEFAULT) {
    stopifnot(tz %in% OlsonNames())

    time.unit  <- match.arg(time_unit)

    start <- convertHumanDate2customTZ(start_date, tz)
    end   <- convertHumanDate2customTZ(end_date, tz)

    seq(
      from = start,
      to   = end,
      by   = ifelse(
        test = is.null(num_seq),
        yes  = convertTimeUnitAsSec(time.unit),
        no   = as.numeric(difftime(end, start, tz, units = 'sec') / num_seq)
      )
    ) %>%
      return()
  }

countTimeSlot <- function(start_date, end_date, time_unit) {
  timeSequenceEncored(start_date, end_date, time_unit) %>%
    length() %>%
    return()
}

convertTimeUnitAsSec <- function(time_unit = getTimeUnitSet()) {
  switch(
    match.arg(time_unit),
    '15min'   = 60 * 15,
    'hourly'  = 60 * 60,
    'daily'   = 60 * 60 * 24,
    'monthly' = 60 * 60 * 24 * 30
  ) %>%
    return()
}

roundTime <- function(time_obj, time_unit, round_func) {
  time.unit.as.sec <- convertTimeUnitAsSec(time_unit)
  first.regular.time.as.sec <-
    round_func(as.numeric(time_obj) / time.unit.as.sec) * time.unit.as.sec

  convertTimestampUTC2HumandateKST(timestamp_ms = first.regular.time.as.sec * 1000) %>%
    return()
}

getQueryTimestamp <- function(date_KST) {
  format(convertHumanDateKST2timestampUTC(date_KST) * 1000,
         scientific = FALSE) %>%
    return()
}

getQueryStartTimestamp <- function(start_date) {
  paste0('start=', getQueryTimestamp(start_date)) %>%
    return()
}

getQueryEndTimestamp <- function(start_date) {
  paste0('end=', getQueryTimestamp(start_date)) %>%
    return()
}

convertTimeUnit2PeriodClass <-
  function(time_unit = getTimeUnitSet()) {
    time.unit <- match.arg(time_unit)
    switch(
      time.unit,
      '15min'  = minutes(15),
      'hourly' = hours(1),
      'daily'  = days(1)
    ) %>%
      return()
  }

sysTimeEncored <- function() {
  Sys.time() %>%
    format(format = '%Y-%m-%d %H:%M:%S') %>%
    return()
}

isTimestampUnitSecond <- function(timestamp) {
  timestamp <- tryCatch(
    expr = as.numeric(timestamp),
    error = function(e) {
      return(NULL)
    },
    warning = function(w) {
      if (w$message == "NAs introduced by coercion")
        return(NULL)
    }
  )

  if (is.null(timestamp)) {
    warning('Please check that the input for timestamp is proper type. Return NULL.')
    return(NULL)
  }

  date.max <- '9999-12-31'
  timsestamp.second.unit.max <-
    date.max %>%
    convertHumanDateKST2timestampUTC() %>%
    convertTimestampUTC2HumandateKST()

  is.unit.second <-
    convertTimestampUTC2HumandateKST(max(timestamp)) < timsestamp.second.unit.max

  return(is.unit.second)
}
