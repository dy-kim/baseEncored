chkLocalFileExistence <-
  function(dir, result_fileName, file_extension) {
    result.file.list <-
      list.files(path = dir,
                 pattern = makeFileNamePattern(result_fileName,
                                               file_extension))
    return(length(result.file.list) > 0)
  }

chkLocalFileExistence_leapOver <-
  function(dir, result_fileName, file_extension) {
    file.name.df <- data.frame(result_fileName  = result_fileName,
                               stringsAsFactors = FALSE)

    exist.logi <- mdply(.data = file.name.df,
                        .fun  = chkLocalFileExistence,
                        dir   = dir) %>%
      rename(file_exists = V1)

    return(exist.logi)
  }

listFilesVectorized <- function(path, pattern, ...) {
  lapply(pattern,
         function(x) {
           list.files(pattern = x, path, ...)
         }) %>%
    unlist() %>%
    return()
}

makeFileNamePattern <- function(file_name, file_extension) {
  pattern <- paste0(file_name, '.*')
  if (!missing(file_extension))
    pattern <- paste0(pattern, '\\', file_extension, '$')

  return(pattern)
}

chkFileExtension <- function(file_name, file_extension) {
  grepl(pattern = paste0('\\.', file_extension),
        file_name) %>%
    return()
}

isCsv <- function(file_name) {
  return(chkFileExtension(file_name, 'csv') |
           chkFileExtension(file_name, 'log'))
}

isRds <- function(file_name) {
  return(chkFileExtension(file_name, 'rds'))
}

writeEmptyFile <- function(file_path) {
  write.table(data.frame(), file_path, col.names = FALSE)
}
