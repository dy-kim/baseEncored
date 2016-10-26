#' @export
isZeroLength <- function(obj) {
  return(length(obj) == 0L)
}

#' @export
isZeroRow <- function(obj_df) {
  if (is.data.frame(obj_df)) {
    return(nrow(obj_df) == 0L)
  } else {
    FORCE_FATAL("Class of 'obj_df' must be a data.frame.")
  }
}

#' @export
returnNullIfZeroRow <-
  function(obj_df, msg = "Return NULL because row size is ZERO!") {
    if (isZeroRow(obj_df)) {
      FORCE_WARN(msg)
      return(NULL)
    }
    return(obj_df)
  }
