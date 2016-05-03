#' @export
installBaseEncored <- 
  function(bleeding_edge = FALSE) {
    org <- 'EncoredTechR'
    
    if (bleeding_edge)
      org <- 'dykim22'
    
    repo <- paste0(org, '/baseEncored')
    install_github(repo)
}
