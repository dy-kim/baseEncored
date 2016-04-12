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

convertHex2Dec <- function(hexa_value) {
  '0x' %>%
    paste0(as.character(hexa_value)) %>%
    as.numeric() -> result
  return(result)
}

convertDec2Hex <- function(dec_value) {
  dec.val <- assureNumeric(dec_value)
  
  if (is.null(dec.val)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  sprintf("%X", dec_value) -> result
  return(result)
}

convertDec2Bin <- function(dec_value) {
  dec.val <- assureNumeric(dec_value)
  if (is.null(dec.val)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  bits <- intToBits(dec.val)
  
  bits %>%
    as.integer() %>%
    rev() %>%
    paste(collapse = "") -> binary.val
  
  options(scipen = nchar(binary.val))
  as.numeric(binary.val) -> result
  return(result)
}

convertBin2Dec <- function(bin_value) {
  bin.val <- assureNumeric(bin_value)
  if (is.null(bin.val)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  as.character(bin.val) %>%
    strsplit(split = "") %>%
    unlist() -> figure
  
  if (!all(figure %in% c("0", "1"))) {
    warning('Invalid binary value. Return NULL.')
    return(NULL)
  }
  
  digits.on <- which(rev(figure) == "1")
  2 ^ digits.on %>%
    sum() -> result
  return(result)
}

divideByThousand <- function(x) {
  return(x / 1000)
}

asNumericAndRound <- function(tbl, digits = 3) {
  suppressWarnings(return(as.numeric(round(tbl, digits))))
}
