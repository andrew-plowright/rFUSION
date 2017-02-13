
# Print messages with package is attached
.onAttach <- function(libname, pkgname) {

  packageStartupMessage("rFUSION - Tools for accessing FUSION functions through R")

  # Check if FUSION path is valid
  if(!file.exists(options()$rFUSION.path)){warning("Path to FUSION directory is invalid\nUse 'FUSIONfolder()' to define valid path")}

}

# Run these functions when package is loaded
.onLoad <- function(libname, pkgname) {

  # Set global options
  op.current <- options()
  op.rFUSION <- list(
    rFUSION.path = "C:\\FUSION",
    rFUSION.printToConsole = TRUE
  )
  toset <- !(names(op.rFUSION) %in% names(op.current))
  if(any(toset)) options(op.rFUSION[toset])
}
