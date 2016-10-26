context("Test updateTimeUnit.PowerUsage")

suppressPackageStartupMessages(library(dplyr))

test_that(desc = "Convert human date to time class with selected timezone",
          code = {
            usage15min <-
              getExtDataFilePathOfInstalledPkg("sample_PowerUsage_multi-sites_15min.rds") %>%
              readRDS()
            usageHourlyUpdated <-
              updateTimeUnit.PowerUsage(usage15min, "hourly")
            usageHourly <-
              getExtDataFilePathOfInstalledPkg("sample_PowerUsage_multi-sites_hourly.rds") %>%
              readRDS()

            getTimeUnit.PowerUsage(usageHourlyUpdated) %>%
              expect_equal(getTimeUnit.PowerUsage(usageHourly))

            getPowerUnit.PowerUsage(usageHourlyUpdated) %>%
              expect_equal(getPowerUnit.PowerUsage(usageHourly))

            expect_equivalent(usageHourlyUpdated, usageHourly)
          })
