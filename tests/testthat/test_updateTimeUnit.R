context("Test updateTimeUnit.PowerUsage")

suppressPackageStartupMessages(library(dplyr))

test_that("Convert human date to time class with selected timezone",
          code = {
            usage15min <-
              readRDS(getExtDataPath("sample_PowerUsage_multi-sites_15min.rds"))
            usageHourlyUpdated <-
              updateTimeUnit.PowerUsage(usage15min, "hourly")
            usageHourly <-
              readRDS(getExtDataPath("sample_PowerUsage_multi-sites_hourly.rds"))

            expect_equal(
              getTimeUnit.PowerUsage(usageHourlyUpdated),
              getTimeUnit.PowerUsage(usageHourly)
            )
            expect_equal(
              getPowerUnit.PowerUsage(usageHourlyUpdated),
              getPowerUnit.PowerUsage(usageHourly)
            )
            expect_equivalent(usageHourlyUpdated, usageHourly)
          })
