runSWAT2009<-function () {
    Sys.setenv(GFORTRAN_STDIN_UNIT = -1)
    Sys.setenv(GFORTRAN_STDOUT_UNIT = -1)
    Sys.setenv(GFORTRAN_STDERR_UNIT = -1)
    libarch = if (nzchar(version$arch))
        paste("libs", version$arch, sep = "/")
    else "libs"
    swatbin <- "rswat2009.exe"
    system(paste(path.package("SWATmodel"),libarch,swatbin, sep = "/"))
}
