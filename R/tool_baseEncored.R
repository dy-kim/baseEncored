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
