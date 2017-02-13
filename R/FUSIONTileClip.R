#' FUSION tile clip
#'
#' Uses a tile scheme to clip a LAS dataset into tiles
#'
#' @export
#'

FUSIONTileClip <- function(inLAS, tiles, outFolder, prefix = NULL, ground = NULL,
                           biaselev = NULL, zmin = NULL, zmax = NULL, verbose = TRUE){


  for(i in 1:length(tiles)){

    if(verbose) cat("Creating tile", i, "of", length(tiles), "\n")

    tile <- tiles[i,]
    tile.ext <- raster::extent(tile)

    outFile <- file.path(outFolder, paste0(prefix, as.character(tile[["tileName"]]), ".las"))

    # Clip LAS files to extent of tile
    ClipData(InputSpecifier = inLAS,
             SampleFile = outFile,
             MinX = tile.ext@xmin, MinY = tile.ext@ymin,
             MaxX = tile.ext@xmax, MaxY = tile.ext@ymax,
             shape = 0, biaselev = biaselev, zmin = zmin, zmax = zmax, ground = ground)
    }
  }

