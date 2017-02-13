#' FUSION extent rounder
#'
#' This tool computes a vector to be used as the \code{gridxy} input for various \link[=FUSIONtools]{FUSION tools}
#
#' When establishing the extent of a raster, FUSION will align the CENTRE of the raster cells to the limits of the
#' "gridxy" vector, as opposed to the cells' edges. To compensate for this, this tool will create a vector that defines
#' an extent as being a half-cell size smaller in all directions than what is desired. In this way, the raster created
#' by FUSION will have cells whose edges align with the desired extent.
#'
#' @param ext An \code{\link[raster]{Extent}} object.
#' @param cellSize numeric. The anticipated cell size of the raster that will be created by FUSION.
#' @param align character. Defines which corner of \code{ext} the output extent vector will be aligned. Accepts
#' \code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, or \code{"bottomright"}.
#' @param snap numeric. Vector of two numbers. An alternative to \code{align}, by which the output will snap to a pair of coordinates.
#' @param spill If the coordinates of \code{ext} are not multiples of \code{cellSize}, the output will either be rounded inwards
#' when \code{spill} is set to \code{"in"}, or outwards if set to \code{"out"}.
#' @return numeric. A vector of four numbers: X1, Y1, X2, Y2, which can be used as inputs for various \link[=FUSIONtools]{FUSION tools}.
#' @export

FUSIONExtentRounder <- function(ext, cellSize, snap = NULL, align = NULL, spill = "in"){

  # Gatekeeper
  if(!is.null(align)){
    if(!align %in% c("topleft", "topright", "bottomleft", "bottomright")){stop("'align' must be set to 'topleft', 'topright', 'bottomleft' or 'bottomright'")}}
  if(!spill %in% c("in", "out")){stop("'spill' must be set to 'in' or 'out'")}
  if(!is.null(snap) & !is.null(align)){stop("Cannot have settings for both 'align' and 'snap'")}
  if(is.null(snap) & is.null(align)){
    warning("Neither 'align' and 'snap' has been defined by user. Using 'align' = \"topleft\" as default")
    align <- "topleft"}
  if(!is.null(snap)){
    if(class(snap) != "numeric"){stop("'snap' must be a numeric value")}
    if(length(snap) != 2){stop("'snap' must be a numeric vector of length 2")}}

  # SET OUTPUT EXTENT USING 'ALIGN'
  if(!is.null(align)){

    # Adjust for the fact that FUSION puts cell centres on the extent limits as opposed to the cell edges
    ext <- ext - cellSize

    # Get range of X and Y axes
    x.range <- ext@xmax - ext@xmin
    y.range <- ext@ymax - ext@ymin

    # Round ranges according to cell size
    if(spill == "out"){
      x.range.round <- APfun::AProunder(x.range, cellSize, "up")
      y.range.round <- APfun::AProunder(y.range, cellSize, "up")}
    if(spill == "in"){
      x.range.round <- APfun::AProunder(x.range, cellSize, "down")
      y.range.round <- APfun::AProunder(y.range, cellSize, "down")}

    # Set xmin, xmax, ymin and ymax according to desired alignment
    if(grepl("top", align)){
      ymax <- ext@ymax
      ymin <- ext@ymax - y.range.round}
    if(grepl("bottom", align)){
      ymax <- ext@ymin + y.range.round
      ymin <- ext@ymin}
    if(grepl("left", align)){
      xmin <- ext@xmin
      xmax <- ext@xmin + x.range.round}
    if(grepl("right", align)){
      xmin <- ext@xmax - x.range.round
      xmax <- ext@xmax}
  }

  # SET OUPTUT EXTENT USING 'snap'
  if(!is.null(snap)){

    # Round xmin, ymin, xmax and ymax to the cell size and coordinates
    if(spill == "out"){
      xmin <- APfun::AProunder(ext@xmin, cellSize, "down", snap[1])
      ymin <- APfun::AProunder(ext@ymin, cellSize, "down", snap[2])
      xmax <- APfun::AProunder(ext@xmax, cellSize, "up", snap[1])
      ymax <- APfun::AProunder(ext@ymax, cellSize, "up", snap[2])}
    if(spill == "in"){
      xmin <- APfun::AProunder(ext@xmin, cellSize, "up", snap[1])
      ymin <- APfun::AProunder(ext@ymin, cellSize, "up", snap[2])
      xmax <- APfun::AProunder(ext@xmax, cellSize, "down", snap[1])
      ymax <- APfun::AProunder(ext@ymax, cellSize, "down", snap[2])}

    # Adjust for the fact that FUSION puts cell centres on the extent limits as opposed to the cell edges
    xmin <- xmin + (cellSize/2)
    ymin <- ymin + (cellSize/2)
    xmax <- xmax - (cellSize/2)
    ymax <- ymax - (cellSize/2)
  }

  # Return xmin, ymin, xmax and ymax as a vector to be used with FUSION tools
  return(c(xmin, ymin, xmax, ymax))
}

