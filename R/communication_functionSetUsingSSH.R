chkRemoteFileExistence <- function(remote_file,
                                   user,
                                   passwd,
                                   IP_address,
                                   port) {
  cmd.chk.file.existence <- paste('test -e', remote_file,
                                  '&& echo TRUE || echo FALSE')
  remoteRunWrapper(cmd.chk.file.existence,
                   user, passwd, IP_address, port) %>%
    system(intern = TRUE) %>%
    as.logical() %>%
    return()
}

sshpassWrapper <- function(cmd, passwd) {
  paste('sshpass -p', passwd, cmd) %>%
    return()
}

remoteRunWrapper <- function(cmd, user, passwd, IP_address, port) {
  cmd.ssh <- paste(
    'ssh -o StrictHostKeyChecking=no -p',
    port,
    paste0(user, '@', IP_address),
    paste0('\'', cmd, '\'')
  )
  sshpassWrapper(cmd.ssh, passwd) %>%
    return()
}

scpWrapper <- function(port, src, dst) {
  paste('scp -o StrictHostKeyChecking=no -P', port, src, dst) %>%
    return()
}

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
    cmd.chg.dir <- paste('cd', script_dir)
    cmd.run     <- paste(cmd.chg.dir,
                         '&&',
                         command_runScript,
                         '&&',
                         command_postRunScript)

    if (is.null(user_id) & is.null(IP_address)) {
      cmd.ssh <- cmd.run
    } else{
      cmd.ssh <-
        remoteRunWrapper(cmd.run, user_id, passwd, IP_address, port)
      stopifnot(chkConnectionDataServer(IP_address, port))
    }
    cat(msg_priorRun)
    if (DEBUG)
      print(cmd.ssh)
    system(command = cmd.ssh)
  }
  return(TRUE)
}

scpFileToRemote <- function(file_path,
                            remote_dst,
                            user_id,
                            passwd,
                            IP_address,
                            port = 22) {
  cmd.scp <- scpWrapper(port,
                        src  = file_path,
                        dst  = paste0(user_id, '@', IP_address, ':',
                                      remote_dst))
  sshpassWrapper(cmd.scp, passwd) %>%
    system()
}

scpFileFromRemote <- function(remote_fileName,
                              remote_src_dir,
                              local_dst_dir,
                              user_id,
                              passwd,
                              IP_address,
                              port = 22) {
  cmd.scp <- scpWrapper(
    port,
    src  = paste0(
      user_id,
      '@',
      IP_address,
      ':',
      remote_src_dir,
      remote_fileName
    ),
    dst  = local_dst_dir
  )
  sshpassWrapper(cmd.scp, passwd) %>%
    system()
}
