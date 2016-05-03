convertBaseCore <-
  function(input_value,
           input_base = getConvertibleBase(),
           output_base = getConvertibleBase()) {
    stopifnot(chkSysName('Linux'))
    stopifnot(!missing(input_value))
    
    input_base  <- as.character(input_base)
    output_base <- as.character(output_base)
    input.base  <- match.arg(input_base)
    output.base <- match.arg(output_base)
    
    input.val <- ifelse(
      test = input.base == '16',
      yes = toupper(input_value),
      no = assureNumeric(input_value)
    )
    
    if (is.null(input.val)) {
      message('Input value is not numeric. Return NULL.')
      return(NULL)
    }
    
    getBcCodeConvertBase(input.val, input.base, output.base) %>%
      bcInterface() -> result
    
    result <-
      ifelse(test = output.base == '10',
             yes = asNumericFullSignificantFigure(result),
             no = result)
    return(result)
  }

getBcCodeConvertBase <- function(val, ibase, obase) {
  if (ibase == 2 & obase == 10)
    obase <- 1010
  
  if (ibase == 2 & obase == 16)
    obase <- 10000
  
  if (ibase == 16 & obase == 10)
    obase <- 'A'
  
  paste(
    paste0('ibase=', formatNonSci(ibase)),
    paste0('obase=', formatNonSci(obase)),
    formatNonSci(val),
    sep = ';'
  ) -> code
  
  return(code)
}

bcInterface <- function(code) {
  paste0("echo '", code, "' | bc") %>%
    system(intern = TRUE) -> result
  return(result)
}

convertBase <-
  Vectorize(FUN = convertBaseCore,
            vectorize.args = 'input_value')

######################
#  Applied Functions
######################
convertHex2Dec <- function(hexa_value) {
  result <-
    convertBase(
      input_value = hexa_value,
      input_base  = 16,
      output_base = 10
    )
  
  return(result)
}

convertDec2Hex <- function(dec_value) {
  dec.val <- assureNumeric(dec_value)
  
  if (is.null(dec.val)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  result <-
    convertBase(
      input_value = dec.val,
      input_base  = 10,
      output_base = 16
    )
  
  return(result)
}

convertDec2Bin <- function(dec_value) {
  dec.val <- assureNumeric(dec_value)
  
  if (is.null(dec.val)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  result <-
    convertBase(
      input_value = dec.val,
      input_base  = 10,
      output_base = 2
    )
  
  return(result)
}

convertBin2Dec <- function(bin_value) {
  bin.val <- assureNumeric(bin_value)
  
  if (is.null(bin.val)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  result <-
    convertBase(
      input_value = bin.val,
      input_base  = 2,
      output_base = 10
    )
  
  return(result)
}

#####################################
#
#  convertIBASE2OBASE : Old version
#
#####################################
# convertHex2Dec <- function(hexa_value) {
#   '0x' %>%
#     paste0(as.character(hexa_value)) %>%
#     as.numeric() -> result
#   return(result)
# }
#
# convertDec2Hex <- function(dec_value) {
#   dec.val <- assureNumeric(dec_value)
#
#   if (is.null(dec.val)) {
#     message('Input value is not numeric. Return NULL.')
#     return(NULL)
#   }
#
#   return(result)
# }
#
# convertDec2Bin <- function(dec_value) {
#   dec.val <- assureNumeric(dec_value)
#   if (is.null(dec.val)) {
#     message('Input value is not numeric. Return NULL.')
#     return(NULL)
#   }
#   bits <- intToBits(dec.val)
#
#   bits %>%
#     as.integer() %>%
#     rev() %>%
#     paste(collapse = "") -> binary.val
#
#   options(scipen = nchar(binary.val))
#   as.numeric(binary.val) -> result
#   return(result)
# }
#
# convertBin2Dec <- function(bin_value) {
#   bin.val <- assureNumeric(bin_value)
#   if (is.null(bin.val)) {
#     message('Input value is not numeric. Return NULL.')
#     return(NULL)
#   }
#
#   as.character(bin.val) %>%
#     strsplit(split = "") %>%
#     unlist() -> figure
#
#   if (!all(figure %in% c("0", "1"))) {
#     warning('Invalid binary value. Return NULL.')
#     return(NULL)
#   }
#
#   digits.on <- which(rev(figure) == "1")
#   2 ^ digits.on %>%
#     sum() -> result
#   return(result)
# }
