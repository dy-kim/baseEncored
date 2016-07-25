#' @export
chkLocalFileExistence <-
  function(dir, result_fileName, file_extension) {
    resultFileList <-
      list.files(path = dir,
                 pattern = makeFileNamePattern(result_fileName,
                                               file_extension))
    return(length(resultFileList) > 0)
  }

#' @export
chkLocalFileExistence_leapOver <-
  function(dir, result_fileName, file_extension) {
    fileNameDf <- data.frame(result_fileName  = result_fileName,
                             stringsAsFactors = FALSE)

    result <-
      plyr::mdply(.data = fileNameDf,
                  .fun  = chkLocalFileExistence,
                  dir   = dir) %>%
      rename(file_exists = V1)

    return(result)
  }

#' @export
listFilesVectorized <- function(path, pattern, ...) {
  lapply(pattern,
         function(x) {
           list.files(pattern = x, path, ...)
         }) %>%
    unlist() -> result

  return(result)
}

#' @export
makeFileNamePattern <- function(file_name, file_extension) {
  pattern <- paste0(file_name, ".*")

  if (!missing(file_extension))
    pattern <- paste0(pattern, "\\", file_extension, "$")

  return(pattern)
}

#' @export
chkFileExtension <- function(file_name, file_extension) {
  grepl(pattern = paste0("\\.", file_extension), file_name) -> result
  return(result)
}

#' @export
isCsv <- function(file_name) {
  return(chkFileExtension(file_name, "csv") |
           chkFileExtension(file_name, "log"))
}

#' @export
isRds <- function(file_name) {
  return(chkFileExtension(file_name, "rds"))
}

#' @export
writeEmptyFile <- function(file_path) {
  write.table(data.frame(), file_path, col.names = FALSE)
}
