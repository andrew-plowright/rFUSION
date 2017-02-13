################################################
###  NOTES FOR ADDING NEW FUNCTIONS TO THIS FILE
################################################

# FUSION tools take both INPUTS and SWITCHES. Inputs are generally required (but not always), and usually
# indicate the tools' input data, output data and a few key tool parameters. SWITCHES are optional, and
# are used to control additional tool parameters.
#
# INPUTS and SWITCHES are formatted differently. Each of the functions below contains an "inputs" object
# which determines which of the function's arguments are INPUTS. All other arguments will be considered
# to be SWITCHES, and will be formatted accordingly.
#
# When adding a new function, keep the following things in mind:
#
# - The function's names must be the same as the executable's name. Ex.: "CanopyModel" will call "CanopyModel.exe".
# - The arguments of these functions must be the same as the input and switch names of the FUSION tool,
#   as indicated in the FUSION manual (http://forsys.cfr.washington.edu/fusion/FUSION_manual.pdf)
# - The function's arguments should be put in the same order as the INPUTS and SWITCHES of the FUSION tool.

#' FUSION tools
#'
#' Functions for calling FUSION tools. Consult the \href{http://forsys.cfr.washington.edu/fusion/FUSION_manual.pdf}{FUSION manual}
#' for more details on these tools, their inputs and their outputs. FUSION must be installed and the \code{rFUSION.path}
#' global option must direct to the FUSION folder in which all tool executable files are stored. The
#' \code{\link{FUSIONfolder}} function can be used to define this path.
#'
#' FUSION is created and maintained by Robert J. McGaughey, of the U.S. Forest Service.
#'
#' @name FUSIONtools
#'
#' @param cellsize numeric. Cell size of an output raster file.
#' @param xyunits character. \code{"m"} for meters, \code{"f"} for feet.
#' @param zunits character. \code{"m"} for meters, \code{"f"} for feet.
#' @param coordsys numeric. Geographic coordinat system. 0 for unknown, 1 for UTM, 2 for state plane.
#' @param zone numeric. Coordinate system zone. 0 for unknown.
#' @param horizdatum numeric. Horizontal datum. 0 for unknown, 1 for NAD27, 2 for NAD83
#' @param vertdatum numeric. Vertical datum. 0 for unknown, 1 for 1 for NGVD29, 2 for NAVD88,3 for GRS80.
#' @param ground A PLANS format DTM file representing a digital elevation model (DEM). Generally used to
#' normalize a point cloud to height above ground.
#' @param class numeric. Used to restrict a function to certain classified points.
#' @param gridxy numeric. Vector of four numbers: X1, Y1, X2, Y2. Force the origin of an output raster (lower left corner)
#' to be (X1,Y1) instead of computing an origin from the data extents and force the upper right corner to be (X2, Y2).
#' X2 and Y2 will be rounded up to a multiple of \code{cellsize}. \strong{WARNING:} the \emph{center} of the outpur raster
#' cells will be aligned to the vector defined by \code{gridxy}, not the cell edges. To facilitate the use of \code{gridxy},
#' use the \code{\link{FUSIONExtentRounder}} function.
#' @param align A PLANS format DTM file to which an output raster will be aligned. Same behavior as \code{gridxy}, except
#' parameters are read from the DTM file.
#' @param extent A PLANS format DTM file. Force the origin and extent of the output grid to match the lower left corner
#' and extent of the specified DTM file but adjust the origin to be an even multiple of the cell size and the width and
#' height to be multiples of the cell size.
#' @references FUSION created by Robert J. McGaughey. U.S. Department of Agriculture, Forest
#' Service, Pacific Northwest Research Station, University of Washington, Box 352100,
#' Seattle, WA 98195-2100, \email{bmcgaughey@fs.fed.us}
#' @references FUSION website: \url{http://forsys.cfr.washington.edu/fusion/fusionlatest.html}

NULL

#' @describeIn FUSIONtools Convert an ASC file to a PLANS format DTM file
#' @export

ASCII2DTM <- function(surfacefile,
                      xyunits = "m", zunits = "m", coordsys = 0, zone = 0, horizdatum = 0, vertdatum = 0,
                      Gridfile, multiplier = NULL, offset = NULL){

  # Set names of inputs
  inputs <- c("surfacefile", "xyunits", "zunits", "coordsys", "zone", "horizdatum", "vertdatum", "Gridfile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Generate a Canopy Height Model (CHM) from a point cloud
#' @export

CanopyModel <- function(surfacefile, cellsize, datafile,
                        xyunits = "m", zunits = "m", coordsys = 0, zone = 0, horizdatum = 0, vertdatum = 0,
                        median = NULL, smooth = NULL, texture = NULL, slope = NULL, aspect = NULL,
                        outlier = NULL, multiplier = NULL, class = NULL, ground = NULL, ascii = NULL,
                        grid = NULL, gridxy = NULL, align = NULL, extent = NULL, surface = NULL,
                        peaks = NULL, pointcount = NULL){

  # Set names of inputs
  inputs <- c("surfacefile", "cellsize", "xyunits", "zunits", "coordsys", "zone", "horizdatum", "vertdatum", "datafile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Clip a point cloud to a rectangular extent
#' @export

ClipData <- function(InputSpecifier, SampleFile, MinX, MinY, MaxX, MaxY,
                     shape = NULL, decimate = NULL, ground = NULL, zmin = NULL, zmax = NULL, zpercent = NULL,
                     height = NULL, timemin = NULL, timemax = NULL, zero = NULL, biaselev = NULL, return = NULL,
                     class = NULL, line = NULL, noindex = NULL, lndex = NULL, lda = NULL,
                     nooffset = NULL, precision = NULL){

  # Set names of inputs
  inputs <- c("InputSpecifier", "SampleFile", "MinX", "MinY", "MaxX", "MaxY")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Produce a CSV file of cloud metrics for a point cloud
#' @export

CloudMetrics <- function(InputDataSpecifier, OutputFileName, above = NULL, new = NULL, firstinpulse = NULL,
                         firstreturn = NULL, highpoint = NULL, subset = NULL, id = NULL, minht = NULL,
                         maxht = NULL, outlier = NULL, strata = NULL, intstrata = NULL, kde = NULL, rgb = NULL){

  # Set names of inputs
  inputs <- c("InputDataSpecifier", "OutputFileName")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}

#' @describeIn FUSIONtools Converts data stored in comma separated value (CSV) format into ASCII raster format
#' @export

CSV2Grid <- function(inputfile, column, outputfile,
                      multiplier = NULL, ndzero = NULL){

  # Set names of inputs
  inputs <- c("inputfile", "column", "outputfile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Convert a PLANS format DTM file to an ASC file
#' @export

DTM2ASCII <- function(inputfile, outputfile,
                      csv = NULL, raster = TRUE, multiplier = NULL){

  # Set names of inputs
  inputs <- c("inputfile", "outputfile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Convert a PLANS format DTM file to a TIF file
#' @export

DTM2TIF <- function(inputfile, outputfile = NULL,mask = NULL){

  # Set names of inputs
  inputs <- c("inputfile", "outputfile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}

#' @describeIn FUSIONtools Computes a series of descriptive statistics for a LIDAR data set
#' @export

GridMetrics <- function(groundfile, heightbreak, cellsize, outputfile, datafile,
                        outlier = NULL, class = NULL, id = NULL, minpts = NULL, minht = NULL, nocsv = NULL,
                        noground = NULL, nointdtm = NULL, diskground = NULL, alldensity = NULL, first = NULL,
                        intensity = NULL, nointensity = NULL, rgb = NULL, fuel = NULL, grid = NULL,
                        gridxy = NULL, align = NULL, extent = NULL, buffer = NULL, cellbuffer = NULL,
                        strata = NULL, kde = NULL, ascii = NULL, topo = NULL, raster = NULL){

  # Set names of inputs
  inputs <- c("groundfile", "heightbreak", "cellsize", "outputfile", "datafile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Create a gridded surface (i.e.: a raster) from a point cloud
#' @export

GridSurfaceCreate <- function(surfacefile, cellsize, datafile,
                              xyunits = "m", zunits = "m", coordsys = 0, zone = 0, horizdatum = 0, vertdatum = 0,
                              median = NULL, smooth = NULL, slope = NULL, spike = NULL, gridxy = NULL,
                              align = NULL, extent = NULL, residuals = NULL, class = NULL, minimum = NULL){

  # Set names of inputs
  inputs <- c("surfacefile", "cellsize", "xyunits", "zunits", "coordsys", "zone", "horizdatum", "vertdatum", "datafile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Filter out ground points for a point cloud
#' @export

GroundFilter <- function(outputfile, cellsize, datafile,
                         surface = NULL, median = NULL, smooth = NULL, finalsmooth = NULL, outlier = NULL,
                         gparam = NULL, wparam = NULL, aparam = NULL, bparam = NULL,
                         tolerance = NULL, iterations = NULL, class = NULL, extent = NULL, trim = NULL,
                         diagnostics = NULL, lda = NULL, precision = NULL){

  # Set names of inputs
  inputs <- c("outputfile", "cellsize", "datafile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))
}


#' @describeIn FUSIONtools Convert a LDA or LAS file to an ASCII text file
#' @export

LDA2ASCII <- function(InputFile, OutputFile, format){


  # Set names of inputs
  inputs <- c("InputFile", "OutputFile", "format")

# Execute tool
executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))}


#' @describeIn FUSIONtools Clip a point cloud to a shapefile (SHP)
#' @export

PolyClipData <- function(Polyfile, Outputfile, Datafile, multifile = NULL, class = NULL){

  # Set names of inputs
  inputs <- c("Polyfile", "Outputfile", "Datafile")

  # Execute tool
  executeFunction(toolName = as.character(match.call()[[1]]),  inputs = inputs, formals = names(formals()))}
