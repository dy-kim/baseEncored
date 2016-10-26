context("Test timeSequenceEncored")

testEnvFileName <- "testEnv_timeSequenceEncored.RData"
testEnvFilePath <- getExtDataFilePathOfInstalledPkg(testEnvFileName)

load(testEnvFilePath)

test_that(desc = "Test timeSequenceEncored",
          code = {
            testResult <-
              timeSequenceEncored(start_date, end_date, time_unit, num_seq)

            if (getStatusOfProducingTestAnswer(returnStatus = TRUE)) {
              answer <- testResult
              save(list = ls(),
                   file = getExtDataFilePathOfDevelopingPkg(testEnvFileName))
            }

            expect_equal(testResult, answer)
          })
