context('Converting time')

test_that("Convert human date to time class with selected timezone", {
  expect_equal(
    convertHumanDate2customTZ(date = '2016-04-11',
                              time_zone = 'Asia/Seoul'),
    as.POSIXct('2016-04-11',
               tz = 'Asia/Seoul',
               origin = '1970-01-01')
  )
})

test_that("Convert human date in Asia/Seoul timezone to unix timestamp in UTC timezone",
          {
            expect_equal(convertHumanDateKST2timestampUTC('1970-01-01 09:00:00'), 0)
          })

test_that("Convert unix timestamp in UTC timezon to human date in custom timezone",
          {
            expect_equal(
              convertTimestampUTC2HumandateCustomTZ(0, 'Asia/Seoul'),
              convertHumanDate2KST('1970-01-01 09:00:00')
            )
          })

test_that("Convert unix timestamp in UTC timezon to human date in custom timezone",
          {
            expect_equal(
              convertTimestampUTC2HumandateKST(0),
              convertHumanDate2KST('1970-01-01 09:00:00')
            )
          })

test_that("Convert timezone", {
  expect_equal(
    convertTimezone(convertHumanDate2UTC('1970-01-01'), TZ_to = 'Asia/Seoul'),
    convertHumanDate2KST('1970-01-01 09:00:00')
  )
})

test_that("Convert UTC date in JSON type to time class", {
  expect_equal(
    convertJsonDateEncored('2016-01-01T00:00:00Z', 'Asia/Seoul'),
    convertHumanDate2KST('2016-01-01 09:00:00')
  )
})

test_that("Get time sequence", {
  expect_equal(
    timeSequenceEncored(
      start_date = '2016-01-01',
      end_date = '2016-01-01 02:00',
      time_unit = 'hourly'
    ),
    convertHumanDate2KST(c(
      '2016-01-01 00:00',
      '2016-01-01 01:00',
      '2016-01-01 02:00'
    ))
  )
})

test_that("Count number of time slots in given period", {
  expect_equal(
    countTimeSlot(
      start_date = '2016-01-01 00:00',
      end_date = '2016-01-01 02:00',
      time_unit = 'hourly'
    ),
    length(c(
      '2016-01-01 00:00', '2016-01-01 01:00', '2016-01-01 02:00'
    ))
  )
})

test_that("Convert custom time unit as second", {
  expect_equal(convertTimeUnitAsSec('15min'), 900)
  expect_equal(convertTimeUnitAsSec('hourly'), 3600)
  expect_equal(convertTimeUnitAsSec('daily'), 86400)
})

test_that("Round, floor, ceiling a time object with custom time units; 15min, hour, day",
          {
            expect_equal(
              roundTime(
                time_obj = convertHumanDate2KST('2016-01-01 00:08:00'),
                time_unit = '15min',
                round_func = round
              ),
              convertHumanDate2KST('2016-01-01 00:15:00')
            )
            expect_equal(
              roundTime(
                time_obj = convertHumanDate2KST('2016-01-01 00:08:00'),
                time_unit = '15min',
                round_func = floor
              ),
              convertHumanDate2KST('2016-01-01 00:00:00')
            )
            expect_equal(
              roundTime(
                time_obj = convertHumanDate2KST('2016-01-01 00:08:00'),
                time_unit = '15min',
                round_func = ceiling
              ),
              convertHumanDate2KST('2016-01-01 00:15:00')
            )
          })

test_that("Check if timestamp unit is second", {
  expect_equal(isTimestampUnitSecond(as.numeric(Sys.time())), TRUE)
  expect_equal(isTimestampUnitSecond(as.numeric(Sys.time()) * 1000), FALSE)
})
