grepMulti <- function(pattern, x) {
  if (any('*' %in% pattern)) {
    grep(pattern, x) %>%
      return()
  } else {
    paste0('^', pattern, '$') %>%
      llply(.fun = grep, x = x, perl = TRUE) %>%
      unlist() %>%
      return()
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
      unlist() %>%
      return()
  }

  pattern %>%
    llply(
      .fun = function(pattern, x) {
        which(pattern == x)
      },
      x = x
    ) %>%
    unlist() %>%
    return()
}
