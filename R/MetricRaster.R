#' Metric Raster
#'
#' Convert grid metrics from a CSV file into multiple rasters
#'
#' @param inCSV character. Path for input CSV created from the \code{GridMetrics} function
#' @param outFolder character. Output folder
#' @param metrics character. Names of metrics as they appear in the \code{Abbrev} column in the \code{metricNames}
#' table
#' @param tileName character. Optional name for output files. If set to NULL, output raster files will be given the
#' name of the metric. If \code{tileName} is given a value, then each raster file will be put in a folder named
#' after its metric, and the file itself will be given the \code{tileName} value.
#' @param subHgtBreak logical. If FALSE, output files involving height breaks will use the placeholder "minHeight",
#' if set to TRUE, "minHeight" will be substituted by its corresponding numerical height break value.
#'
#' @export

MetricRaster <- function(inCSV, outFolder, metrics, tileName = NULL, subHgtBreak = TRUE){

  ## Gatekeeper

    inHeaders <- colnames(data.table::fread(inCSV, nrows = 0))

    # Check that 'metrics' are listed in 'metricNames'
    nameFound <- metrics %in% metricNames[,"Abbrev"]
    if(any(!nameFound)) stop("Could not find the following metrics in the 'metricNames' list:\n  ", paste(metrics[!nameFound], collapse = "\n  "))
    metricTable <- subset(metricNames, Abbrev %in% metrics)

  ## Identify height break

    # Find headers that end with "above" followed by a number
    heightBreakHeaders <- inHeaders[grep("above [0-9.]+$", inHeaders)]

    # Remove all non-numeric and non-period characters from header strings to get the height break
    heightBreak <- unique(gsub("[^0-9.]", "", heightBreakHeaders))

    # Replace 'minHeight' with height break
    metricTable[,"FullName"]               <- gsub("minHeight", heightBreak, metricTable[,"FullName"])
    if(subHgtBreak) metricTable[,"Abbrev"] <- gsub("minHeight", heightBreak, metricTable[,"Abbrev"])

  ## Find metric

    # Find matching columns
    metricTable[["col"]] <- match(metricTable[,"FullName"], inHeaders)

    # Check if all metrics were found
    if(any(is.na(metricTable[,"col"]))) stop("Could not find the following metrics in CSV file:\n  ", paste(metricTable[is.na(metricTable[,"col"]), "FullName"], collapse = "\n  "))

  ## Generate metric rasters

    # Define output files
    if(is.null(tileName)){

      metricTable[["outFile"]] <- file.path(outFolder, paste0(metricTable[,"Abbrev"], ".tif"))

    }else{

      metricFolders <- file.path(outFolder, metricTable[,"Abbrev"])
      for(metricFolder in metricFolders) dir.create(metricFolder, showWarnings = FALSE)

      metricTable[["outFile"]] <- file.path(metricFolders, paste0(tileName, ".tif"))
    }

    # Temporarily silence 'print to console'
    oldVal <- FUSIONprintToConsole()
    FUSIONprintToConsole(FALSE)

    # Generate rasters
    for(i in 1:nrow(metricTable)){

      cat("Generating:", metricTable[i, "FullName"], "\n")

      CSV2Grid(inputfile  = inCSV,
               column     = as.numeric(metricTable[i, "col"]),
               outputfile = metricTable[i,"outFile"])
    }

    FUSIONprintToConsole(oldVal)
}


