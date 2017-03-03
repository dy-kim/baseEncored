#' Extract function input while running test code
#'
#' @param functionName A string.
#' @param testPkg Same as \code{pkg} in \code{\link[devtools]{test}}. Package description, can be path or package name. See \code{\link[devtools]{as.package}} for more information
#' @param baseTestCode Same as \code{filter} in \code{\link[devtools]{test}}. Only tests with file names matching this regular expression will be executed. Matching will take on the file name after it has been stripped of "test-" and ".R"
#' @param cache Directory in which to store exported function input
#'
#' @export
extractFunctionInputFromTest <- function(functionName,
                                         testPkg = ".",
                                         baseTestCode,
                                         cache) {
  cache <- normalizePath(cache)
  if (!dir.exists(cache)) {
    stop("Not existing folder.")
  }

  actualTestCodePath <- getTestCodePath(testPkg, baseTestCode)
  actualTestCodeName <-
    tools::file_path_sans_ext(basename(actualTestCodePath))
  testInputCache <- file.path(cache, actualTestCodeName)
  if (!dir.exists(testInputCache)) {
    dir.create(testInputCache, recursive = TRUE)
  }

  f <- match.fun(functionName)
  if (!isFunctionInputCaptured(f, testInputCache)) {
    printTestStartInstruction(testInputCache)
    stop("Please follow above information.")
  }

  if (!isFuncEnvironFileExisting(testInputCache, functionName)) {
    setTargetedFunction(functionName, optionName = SAVE_FUNC_ENVIRON)
    testthat::test_file(path = actualTestCodePath, reporter = "list")
    unsetTargetedFunction(functionName, optionName = SAVE_FUNC_ENVIRON)
  }

  if (!isFuncEnvironFileExisting(testInputCache, functionName)) {
    FORCE_FATAL("Function %s is not in the test %s.R!",
                functionName,
                actualTestCodeName)
  }

  envFilePath <- findFuncEnvironFile(testInputCache, functionName)
  return(envFilePath)
}

#' @importFrom testthat find_test_scripts
getTestCodePath <- function(testPkg, baseTestCode) {
  path <-
    testthat::find_test_scripts(path = file.path(testPkg, "tests", "testthat"),
                                filter = baseTestCode)
  stopifnot(length(path) == 1L)
  return(path)
}

isFunctionInputCaptured <- function(f, testInputCache) {
  answer <-
    getFunctionInputCapturingCode(testInputCache) %>%
    removeAllWhitespace()
  test <- getFirstLineOfFunction(f) %>% removeAllWhitespace()
  return(identical(answer, test))
}

getFunctionInputCapturingCode <- function(testInputCache) {
  sprintf("baseEncored::saveFunctionEnvironment(savePath = \"%s\")",
          testInputCache)
}

#' @importFrom stringr str_replace_all
removeAllWhitespace <- function(x) {
  stringr::str_replace_all(x, " ", "")
}

printTestStartInstruction <- function(testInputCache) {
  FORCE_INFO("Insert following line to the first line of the function definition.")
  FORCE_INFO(getFunctionInputCapturingCode(testInputCache))
}

getFirstLineOfFunction <- function(f) {
  funcDefinition <- deparse(f)
  firstLineIndex <-
    min(findIndexOfLeftCurlyBraces(funcDefinition)) + 1L

  return(funcDefinition[firstLineIndex])
}

findIndexOfLeftCurlyBraces <- function(x) {
  grep(pattern = "\\{", x = x)
}
