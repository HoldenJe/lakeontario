### FEED Functions

#' feed_convert_coords
#' @description FEED stores coordinate as `DDMM.ddd`. This function converts
#' that format to `DD.ddd` for plotting purposes. Note: Longitude will normally require
#' the value to be multiplied by `-1 ` for sites in the western hemisphere
#' @param feedcoord a lat or long in the FEEd format of DDMM.mmm
#' @returns coordinate value as DD.ddd
#' @export
#' @examples
#' feed_convert_coords(7705.819) * (-1)
#' feed_convert_coords(4402.984)

feed_convert_coords <- function(feedcoord){
  feedcoord <- as.character(feedcoord)
  deg <- as.numeric(substr(feedcoord, 1,2))
  mm <- as.numeric(substr(feedcoord, 3, nchar(feedcoord)))/60
  DDddd <- deg + mm
  DDddd
}

