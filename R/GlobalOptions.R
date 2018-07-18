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

PrintToConsole <- function(value = NULL){

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

#' Adjust "grid XY"
#'
#' The \code{gridxy} argument, can be used to define the extent of rasters output by several FUSION functions.
#' However, when FUSION functions are run from the command line, the \emph{centre} of the outmost cells will
#' align with the coordinates supplied by \code{gridxy}. When \code{GridXYAdjust} is set to TRUE
#' (which it is by default), an automatic adjustment will be made so that the OUTER EDGE of the cells
#' aligns with the coordinates instead. This applies to all FUSION functions using the \code{gridxy} argument.
#'
#' @param value logical. If set to TRUE, all values used as inputs for the \code{gridxy} argument will
#' be adjusted before being passed to the FUSION command line functions, in order for the output raster's edges
#' to align with \code{gridxy}.
#' @return If no input is given, the current value of the \code{rFUSION.grixyAdjust} global option is returned
#' @export

GridXYAdjust <- function(value = NULL){

  if(is.null(value)){

    options()$rFUSION.grixyAdjust

  }else{

    if(!is.logical(value)){

      stop("Input must be TRUE or FALSE")

    }else{

      options(rFUSION.grixyAdjust = value)

    }
  }
}
