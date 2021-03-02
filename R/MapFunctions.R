#' Lake Ontario Base Map
#' @description Ted Schaner created a simple csv file that contains the Lake Ontario shoreline
#' and the 50, 100 and 150m bathymetric contours. This function plots the base map upon which
#' additional data layers can be plotted.
#' @export

SchanerMap<-function() {
  old.par <- par(no.readonly=TRUE)
  par(mar=c(0.1,0.1,0.1,0.1), bty='n')

  plot(lat~lon, LOschaner, subset=(grp=='shore'), type='l', xaxt='n', yaxt='n')
  points(lat~lon, LOschaner, subset=(grp=='m50'), type='l', lty=2)
  points(lat~lon, LOschaner, subset=(grp=='m100'), type='l', lty=2)
  points(lat~lon, LOschaner, subset=(grp=='m150'), type='l', lty=2)

  #on.exit(par(old.par))
}

#' Lake Ontario Spatial Map
#' @description This implemetation of the Lake Ontario base map utilizes
#' shape files uploaded within this package. This function requires
#' that the package `sp` is installed.
#'
#' @export
#' @seealso plot(LOshore)

LOspatial<-function(){
  if(!requireNamespace('sp')){
    stop('sp package required for this function.
         install.packages("sp")')
  }
  require(sp)
  old.par <- par(no.readonly=TRUE)
  par(mar=c(0.1,0.1,0.1,0.1), bty='n')
  plot(LOshore)
  plot(LObath100m, add=T, lty=2)
  #on.exit(par(old.par))
  }



