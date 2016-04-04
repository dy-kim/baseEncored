convertHex2Dec <- function(hexa_value) {
  '0x'%>%
    paste0(as.character(hexa_value)) %>%
    as.numeric() %>%
    return()
}

convertDec2Hex <- function(dec_value) {
  sprintf("%X", dec_value) %>%
    return()
}

convertDec2Bin <- function(dec_value) {
  intToBits(dec_value) %>%
    as.integer() %>%
    rev() %>%
    paste(collapse = "") -> binary.val

  options(scipen = nchar(binary.val))
  as.numeric(binary.val) %>%
    return()
}

convertBin2Dec <- function(bin_value) {
  as.character(bin_value) %>%
    strsplit(split = "") %>%
    unlist() -> figure

  if (!all(figure %in% c("0", "1"))) {
    warning('Invalid binary value. Return NULL.')
    return(NULL)
  }

  2^which(rev(figure) == "1") %>%
    sum() %>%
    return()
}
