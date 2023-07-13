#' @title shape_ontarioshore
#' @name shape_ontarioshore
#'
#' @description Lake Ontario as shapefile
#'
#' @format an sf spatial data frame
#'
#' @examples
#' \dontrun{
#' data("shape_ontarioshore")
#' class(shape_ontarioshore)
#' plot(shape_ontarioshore)
#' }
#' @seealso base_ontarioshore
#'

data("shape_ontarioshore")
shape_ontarioshore


#' base_ontarioshore
#' @name base_ontarioshore
#' @description Lake Ontario as base ggplot object
#'
#' @format a ggplot object with the strata as a geom_sf() object
#'
#' @examples
#' \dontrun{
#' data("base_ontarioshore")
#' base_ontarioshore
#' }
#'
#' @seealso shape_ontarioshore

data("base_ontarioshore")
base_ontarioshore
