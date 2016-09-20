#' @export
installBaseEncored <- function(bleeding_edge = FALSE) {
  org <- "EncoredTechR"

  if (bleeding_edge)
    org <- "dy-kim"

  repo <- paste0(org, "/", "baseEncored")
  devtools::install_github(repo)
}

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
      if (w$message == "NAs introduced by coercion")
        return(NULL)
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
  stopifnot(is.matrix(x) | is.data.frame(x))
  matrixAsVector <- as.vector(t(x))
  result <- vectorizedMedian(matrixAsVector, ncol(x))
  rm(matrixAsVector)
  gc()
  return(result)
}

getExtDataPath <- function(filename) {
  system.file("extdata",
              filename,
              package = "baseEncored")
}

# nolint start
# dumpUsageForTest <- function(siteId,
#                              timeUnit,
#                              powerUnit) {
#   JediETL::dumpFeederUsageEncoredAPI(siteId,
#                                      timeUnit,
#                                      powerUnit,
#                                      "2016-09-01 00:00",
#                                      "2016-09-01 03:00") %>%
#     filter(local_feeder_id == 0) %>%
#     select(site_id, device_id, date, usage) %>%
#     PowerUsage(timeUnit, powerUnit)
# }
# nolint end
