#' Multiple pattern matching.
#'
#' \code{grepMulti} matches \code{pattern}, in this case a user name(s)/appliance(s),
#' to a character vector \code{x} of all users/appliances, respectively.
#'
#' @param pattern a character string to be matched in the given character vector.
#' @param x a character vector to be matched to.
#'
#' @return \code{grepMulti} returns a vector of the indices of \code{x} that
#' matched with the elements of \code{pattern}.
#'
#' @seealso
#' \code{\link{grepMetaData}}: searches meta data given a targeted user and/or
#' appliance and returns the selected row of data. \cr
#' \code{\link{searchMetaData}}
#'
#' @examples
#' user.name <- c('David')
#' appliance.name <- c('Fridge')
#' user.list <- c('Kanye', 'David', 'John')
#' appliance.list <- c('AirWasher', 'Fridge', 'MicroOven', 'Fridge')
#'
#' grepMulti(user.name, user.list) # 2
#' grepMulti(appliance.name, appliance.list) # 2 4
#'
#' @export
grepMulti <- function(pattern, x) {
  if (any("*" %in% pattern)) {
    grep(pattern, x) -> result
    return(result)
  } else {
    paste0("^", pattern, "$") %>%
      plyr::llply(.fun = grep, x = x, perl = TRUE) %>%
      unlist() -> result
    return(result)
  }
}

#' @export
whichMulti <- function(pattern, x) {
  if (pattern == "*") {
    pattern %>%
      plyr::llply(
        .fun = function(pattern, x) {
          seq(1, length(x))
        },
        x = x
      ) %>%
      unlist() -> result
    return(result)
  }
  pattern %>%
    plyr::llply(
      .fun = function(pattern, x) {
        which(pattern == x)
      },
      x = x
    ) %>%
    unlist() -> result
  return(result)
}

#' @export
divideByThousand <- function(x) {
  return(x / 1000)
}

#' @export
asNumericAndRound <- function(data, digits = 3) {
  suppressWarnings(return(as.numeric(round(data, digits))))
}

#' @export
asNumericFullSignificantFigure <- function(value) {
  expandSciNotation(value)
  return(as.numeric(value))
}

#' @export
assureNumeric <- function(val) {
  result <- tryCatch(
    expr = as.numeric(val),
    error = function(e) {
      return(NULL)
    },
    warning = function(w) {
      if (w$message == "NAs introduced by coercion") {
        return(NULL)
      }
    }
  )
  return(result)
}

#' @export
formatNonSci <- function(value) {
  return(format(value, scientific = FALSE))
}

#' Form Row Medians
#'
#' @param x an array of two dimensions,
#' containing numeric, integer values, or a numeric data frame.
#'
#' Form row medians for numeric arrays (or data frames).
#'
#' @export
rowMedian <- function(x) {
  if (!(is.matrix(x) | is.data.frame(x))) {
    FORCE_FATAL("Argument 'x' must be one of the 'matrix' or 'data.frame' type.")
  }
  matrixAsVector <- as.vector(t(x))
  result <- vectorizedMedian(matrixAsVector, ncol(x))
  rm(matrixAsVector)
  gc()
  return(result)
}

#' @export
getFunctionEnvironmentAtBreakPoint <- function(basePath = getwd()) {
  parentEnv <- parent.frame()
  parentFunctionName <- as.character(sys.call(-1L))[1]
  envFileName <- paste0("funcEnv_", parentFunctionName, ".RData.gz")
  envFilePath <- file.path(basePath, envFileName)

  save(
    list = ls(envir = parentEnv),
    file = envFilePath,
    envir = parentEnv,
    compress = "gzip"
  )
}

#' Start producing test answer
#'
#' @description Set the global option \code{produceTestAnswer} as \code{TRUE}
#' @seealso \code{\link{stopProducingTestAnswer}}, \code{\link{getStatusOfProducingTestAnswer}}
#'
#' @export
startProducingTestAnswer <- function(devPkgPath = getwd()) {
  options(devPkgPath = devPkgPath)
  options(produceTestAnswer = TRUE)
  getStatusOfProducingTestAnswer(FALSE)
}

#' Stop producing test answer
#'
#' @description Set the global option \code{produceTestAnswer} as \code{FALSE}
#' @seealso \code{\link{startProducingTestAnswer}}, \code{\link{getStatusOfProducingTestAnswer}}
#'
#' @export
stopProducingTestAnswer <- function() {
  options(devPkgPath = NULL)
  options(produceTestAnswer = FALSE)
  getStatusOfProducingTestAnswer(FALSE)
}

#' Get the status of producing test answer
#'
#' @description Get the global option \code{produceTestAnswer}
#' @param returnStats A logical controls return status or not
#' @seealso \code{\link{startProducingTestAnswer}}, \code{\link{stopProducingTestAnswer}}
#'
#' @export
getStatusOfProducingTestAnswer <- function(returnStatus = TRUE) {
  status <- getOption("produceTestAnswer")

  if (is.null(status)) {
    # produceTestAnswer: DEFAULT should be FALSE
    status <- FALSE
  }

  FORCE_INFO(paste("Option of producing test answers:", status))
  if (returnStatus) {
    return(status)
  }
}

#' @export
getExtDataFilePathOfInstalledPkg <- function(filename, pkgName) {
  getExtDataFolderPathOfInstalledPkg(pkgName) %>%
    getFilePathFromGivenExtDataFolderPath(filename)
}

#' @export
getExtDataFolderPathOfInstalledPkg <- function(pkgName) {
  if (isPackageAttached(pkgName)) {
    unloadNamespace(pkgName)
    path <- system.file("extdata", package = pkgName)
    suppressPackageStartupMessages(require(
      package = pkgName,
      quietly = TRUE,
      character.only = TRUE
    ))
  } else {
    path <- system.file("extdata", package = pkgName)
  }
  return(path)
}

isPackageAttached <- function(pkgName) {
  paste0("package:", pkgName) %in% search()
}

getFilePathFromGivenExtDataFolderPath <-
  function(folderPath, filename) {
    path <- file.path(folderPath, filename)

    if (file.exists(path)) {
      return(path)
    } else {
      FORCE_WARN(paste0("File does not exists: '", path, "'"))
      return("")
    }
  }

#' @export
getExtDataFilePathOfDevelopingPkg <-
  function(filename, pkgName, devPkgPath = getDevPkgPath()) {
    getExtDataFolderPathOfDevelopingPkg(pkgName, devPkgPath) %>%
      getFilePathFromGivenExtDataFolderPath(filename)
  }

#' @export
getExtDataFolderPathOfDevelopingPkg <-
  function(pkgName, devPkgPath = getDevPkgPath()) {
    if (is.null(devPkgPath)) {
      FORCE_FATAL(
        "The argument 'devPkgPath' is NULL. Please set correct path of a package developing folder."
      )
    }

    if (isPkgProj(pkgName, devPkgPath)) {
      path <- file.path(devPkgPath, "inst", "extdata")
      return(path)
    } else {
      paste("Current working directory does not include a",
            pkgName,
            "R package project.") %>%
        FORCE_WARN()
      return("")
    }
  }

isPkgProj <- function(pkgName, path) {
  return(hasPkgDescription(pkgName, path) & hasRproj(pkgName, path))
}

hasPkgDescription <- function(pkgName, path) {
  pathOfDescriptionFile <- file.path(path, "DESCRIPTION")
  if (!file.exists(pathOfDescriptionFile)) {
    return(FALSE)
  }

  pkgNameInDescriptionFile <-
    read.dcf(file = pathOfDescriptionFile, fields = "Package") %>%
    as.character()

  return(pkgName == pkgNameInDescriptionFile)
}

hasRproj <- function(pkgName, path) {
  nameOfRprojFile <- paste0(pkgName, ".Rproj")
  pathOfRprojFile <- file.path(path, nameOfRprojFile)
  return(file.exists(pathOfRprojFile))
}

getDevPkgPath <- function() {
  getOption("devPkgPath")
}

#' @export
saveTestAnswer <- function(obj, ansFileName, pkgName) {
  ansFilePath <-
    getExtDataFolderPathOfDevelopingPkg(pkgName) %>%
    file.path(ansFileName)
  saveRDS(obj, ansFilePath)
  return(ansFilePath)
}

#' @export
getTestAnswerFilePath <- function(ansFileName, pkgName) {
  ansFilePath <-
    getExtDataFilePathOfInstalledPkg(ansFileName, pkgName)

  if (ansFilePath == "") {
    FORCE_FATAL(
      "Make an answer file before the test.
      After making an answer file, please 'Clean and Rebuild'."
    )
  }
  return(ansFilePath)
}
