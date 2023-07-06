.onAttach <- function(libname, pkgname) {
  txt <- paste(c(paste("This is SESraster", utils::packageVersion("SESraster")),
               "To support our work, cite SESraster in your publications. See:",
               '  citation("SESraster")'), collapse="\n")
  packageStartupMessage(txt)
}
