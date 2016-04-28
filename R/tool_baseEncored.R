grepMulti <- function(pattern, x) {
  if (any('*' %in% pattern)) {
    grep(pattern, x) -> result
    return(result)
  } else {
    paste0('^', pattern, '$') %>%
      llply(.fun = grep, x = x, perl = TRUE) %>%
      unlist() -> result
    return(result)
  }
}

whichMulti <- function(pattern, x) {
  if (pattern == '*') {
    pattern %>%
      llply(
        .fun = function(pattern, x) {
          seq(1, length(x))
        },
        x = x
      ) %>%
      unlist() -> result
    return(result)
  }
  
  pattern %>%
    llply(
      .fun = function(pattern, x) {
        which(pattern == x)
      },
      x = x
    ) %>%
    unlist() -> result
    return(result)
}

divideByThousand <- function(x) {
  return(x / 1000)
}

asNumericAndRound <- function(tbl, digits = 3) {
  suppressWarnings(return(as.numeric(round(tbl, digits))))
}

asNumericFullSignificantFigure <- function(value) {
  expandSciNotation(value)
  return(as.numeric(value))
}

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

formatNonSci <- function(value) {
  return(format(value, scientific = FALSE))
}
