#' @export
chkRemoteFileExistence <- function(remote_file,
                                   user,
                                   passwd,
                                   IP_address,
                                   port) {
  cmd <- paste("test -e", remote_file, "&& echo TRUE || echo FALSE")
  remoteRunWrapper(cmd, user, passwd, IP_address, port) %>%
    system(intern = TRUE) %>%
    as.logical() -> result
  return(result)
}

#' @export
sshpassWrapper <- function(cmd, passwd) {
  paste("sshpass -p", passwd, cmd) -> result
  return(result)
}

#' @export
remoteRunWrapper <- function(cmd, user, passwd, IP_address, port) {
  cmd.ssh <- paste(
    "ssh -o StrictHostKeyChecking=no -p",
    port,
    paste0(user, "@", IP_address),
    paste0("'", cmd, "'")
  )
  sshpassWrapper(cmd.ssh, passwd) -> result
  return(result)
}

#' @export
scpWrapper <- function(port, src, dst) {
  paste("scp -o StrictHostKeyChecking=no -P", port, src, dst) -> result
  return(result)
}

#' @export
runRemoteScript <- function(result_fileName,
                            msg_priorRun,
                            command_runScript,
                            command_postRunScript,
                            local_dst_dir,
                            script_dir,
                            user_id,
                            passwd,
                            IP_address,
                            port = 22,
                            DEBUG = FALSE) {
  if (!chkLocalFileExistence(local_dst_dir, result_fileName)) {
    cmdRun     <- paste("cd",
                        script_dir,
                        "&&",
                        command_runScript,
                        "&&",
                        command_postRunScript)
    if (is.null(user_id) & is.null(IP_address)) {
      cmdSsh <- cmdRun
    } else {
      cmdSsh <-
        remoteRunWrapper(cmdRun, user_id, passwd, IP_address, port)
    }
    cat(msg_priorRun)
    if (DEBUG)
      print(cmdSsh)
    system(command = cmdSsh)
  }
  return(TRUE)
}

#' @export
scpFileToRemote <- function(file_path,
                            remote_dst,
                            user_id,
                            passwd,
                            IP_address,
                            port = 22) {
  cmd.scp <- scpWrapper(port,
                        src = file_path,
                        dst = paste0(user_id, "@", IP_address, ":", remote_dst))
  sshpassWrapper(cmd.scp, passwd) %>%
    system()
}

#' @export
scpFileFromRemote <- function(remote_fileName,
                              remote_src_dir,
                              local_dst_dir,
                              user_id,
                              passwd,
                              IP_address,
                              port = 22) {
  paste0(user_id,
         "@",
         IP_address,
         ":",
         remote_src_dir,
         remote_fileName) %>%
    scpWrapper(port, src = ., dst = local_dst_dir) %>%
    sshpassWrapper(passwd) %>%
    system()
}
