######################
#  Applied Functions
######################
#' Convert hexa value to decimal value
#'
#' @param x Hexa vaulues (character type)
#'
#' @export
convertHex2Dec <- function(x) {
  convertBase(x = x,
              inputBase  = "16",
              outputBase = "10")
}

#' Convert decimal value to hexa value
#'
#' @param x Decimal vaulues (numeric or character type)
#'
#' @export
convertDec2Hex <- function(x) {
  decVal <- assureNumeric(x)
  if (is.null(decVal)) {
    message("Input value is not numeric. Return NULL.")
    return(NULL)
  }
  result <- convertBase(x = decVal,
                        inputBase  = "10",
                        outputBase = "16")
  return(result)
}

#' Convert decimal value to binary value
#'
#' @param x Decimal vaulues (numeric or character type)
#'
#' @export
convertDec2Bin <- function(x) {
  decVal <- assureNumeric(x)
  if (is.null(decVal)) {
    message("Input value is not numeric. Return NULL.")
    return(NULL)
  }
  result <- convertBase(x = decVal,
                        inputBase  = "10",
                        outputBase = "2")
  return(result)
}

#' Convert binary value to decimal value
#'
#' @param x Binary vaulues (numeric or character type)
#'
#' @export
convertBin2Dec <- function(x) {
  binVal <- assureNumeric(x)
  if (is.null(binVal)) {
    message("Input value is not numeric. Return NULL.")
    return(NULL)
  }
  result <- convertBase(x = binVal,
                        inputBase  = "2",
                        outputBase = "10")
  return(result)
}
