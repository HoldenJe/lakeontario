#' convert2coorddd
#' @description Parses coordinates from a NMEA string to Deg_DD
#' @param coord A coordinate in the for DDMM.ddd
#' @example
#' convert2coorddd(4351.986)
#' @export

convert2coorddd <- function(coord){
  degree <- as.numeric(substr(coord, 1,2))
  mindd <- as.numeric(substr(coord, 3, 8))/60
  dec_dd <- degree + mindd
  dec_dd
}
