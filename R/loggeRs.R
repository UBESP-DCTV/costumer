loggeRs <- function(
    prj_name,
    n = 1,
    baselevel = 'INFO',
    pathlog = getwd()
){
#
##-----------------------------------------------------------------------------
#
## loggeRs(prj_name, n, baselevel, pathoutput) create n logger object in the
## global environment named logger.1 ... logger.<n> at baselevel level and the
## corresponding n logfiles named <Sys.time()>_<log.names>.log all into the
## pathlog directory
#
## INPUT  : prj_name  = name to assign to teh logfile - character vector
##          n         = number of loggers to create - integer (numerical)
##          baselevel = base level to preset all the loggers created
##          pathlog   = path in which store the logfiles
#
## OUTPUT : _n_ logfiles in the _pathlog_ directory.
#
##=============================================================================
#
## Set the numbers and the suffix of each log file
#
    n.log <- n
    log.names <- paste0(prj_name, as.character(c(1:n.log)))
#
## Set the interested log level each script
## Level details to access: DEBUG > INFO > WARN > ERROR > FATAL
#
    log.levels <- rep(baselevel, n)
    names(log.levels) <- log.names
#
## Path of the log files
#
    log.files <- file.path(
        pathlog,
        paste0(
            as.numeric(Sys.time()), '_', log.names, '.log'
        )
    )
#
## Create the loggers (putting all in a single list)
#
    loggers <- lapply(1:n.log,
                      function(i) {
                          create.logger(
                              logfile = file.path(log.files)[i],
                              level = log.levels[i]
                          )
                      }
    )
#
## Naming the list with the name of the variables to be used
#
    names(loggers) <- paste0("logger.", 1:n.log)
#
## Create the loger variables in the global environment
#
    invisible(list2env(loggers , envir = .GlobalEnv))
}
