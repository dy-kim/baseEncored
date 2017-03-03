#' @importFrom digest digest
#' @export
saveFunctionEnvironment <- function(savePath = getwd(),
                                    prefix = "funcExecEnviron",
                                    algo = "xxhash64") {
  functionName <- as.character(sys.call(-1L))[1]
  if (is.na(functionName)) {
    FORCE_FATAL("This is not a function execution environment.")
  }

  if (!isTargetedFunction(functionName, optionName = SAVE_FUNC_ENVIRON)) {
    FORCE_TRACE("%s(): Function execution environment is not saved.",
                functionName)
    return(invisible(NULL))
  } else {
    FORCE_TRACE("%s(): Function execution environment is saved.",
                functionName)
  }

  funcEnv <- parent.frame()
  key <- digest::digest(funcEnv, algo = algo)
  envFileName <-
    sprintf("%s_%s_%s.rds", prefix, functionName, key)
  envFilePath <- file.path(savePath, envFileName)

  if (!file.exists(envFilePath)) {
    saveRdsAsnyc(object = funcEnv,
                 file = envFilePath,
                 compress = "gzip")
  }

  return(invisible(envFilePath))
}

findFuncEnvironFile <- function(path, functionName) {
  dir(
    path,
    pattern = sprintf("^%s_%s*", "funcExecEnviron", functionName),
    full.names = TRUE
  )
}

isFuncEnvironFileExisting <- function(path, functionName) {
  length(findFuncEnvironFile(path, functionName)) > 0
}

isTargetedFunction <- function(functionName, optionName) {
  targetedFucntion <- getOption(optionName)
  if (is.null(targetedFucntion)) {
    return(FALSE)
  }

  if (functionName %in% targetedFucntion) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

setTargetedFunction <- function(functionName, optionName) {
  oldOption <- getOption(optionName)
  newOption <- c(oldOption, functionName) %>% unique() %>% list()
  names(newOption) <- optionName
  options(newOption)

  return(invisible(oldOption))
}

unsetTargetedFunction <- function(functionName, optionName) {
  oldOption <- getOption(optionName)
  newOption <- setdiff(oldOption, functionName) %>% list()
  names(newOption) <- optionName
  options(newOption)

  return(invisible(oldOption))
}
