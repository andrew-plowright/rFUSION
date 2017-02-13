#' Set FUSION folder path
#'
#' A function to set the path to the FUSION directory. This directory contains the executable files employed by the
#' functions in rFUSION. The default location is \code{C:\\FUSION}. This path is stored as the \code{rFUSION.path}
#' global option.
#'
#' @param path character. A path directing to the FUSION folder
#'
#' @return If no input is given for \code{path}, the current folder set for the \code{rFUSION.path}
#' global option is given.
#' @export

FUSIONfolder <- function(path = NULL){

  if(is.null(path)){

    options()$rFUSION.path

  }else{

    if(!file.exists(path)){stop("Folder does not exist")}

    options(rFUSION.path = path)

  }
}

#' Print to Console
#'
#' A function for changing the \code{rFUSION.printToConsole} global option, which sets whether or not the FUSION tool
#' outputs are printed to the console. If no value is set, this function will return the current value.
#'
#' @param value logical. FUSION tool outputs will print to console if set to \code{TRUE}
#' @return If no input is given, the current value of the \code{rFUSION.printToConsole} global option is returned
#' @export

FUSIONprintToConsole <- function(value = NULL){

  if(is.null(value)){

    options()$rFUSION.printToConsole

  }else{

    if(!is.logical(value)){

      stop("Input must be TRUE or FALSE")

    }else{

      options(rFUSION.printToConsole = value)

    }
  }
}
