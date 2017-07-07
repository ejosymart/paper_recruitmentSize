
validPoints = function(lon, lat, land=FALSE) {

  coords = cbind(lon=lon, lat=lat)
  xind = which(complete.cases(coords))
  coords = coords[xind, ]
  
  land = map(database="worldHires", fill=TRUE)
  land = map2SpatialPolygons(land, 
                           IDs=sapply(strsplit(land$names, ":"), FUN="[", i=1), 
                           proj4string=CRS("+proj=longlat"))
  sets = SpatialPoints(cbind(lon=coords[, "lon"], lat=coords[, "lat"]), proj4string=CRS("+proj=longlat"))
  ind = is.na(over(sets, land))
  
  if(isTRUE(land)) ind = !ind
  ind = xind[which(ind)]
  
  return(ind)
}


# Selectivity functions ---------------------------------------------------
L50 = function(x, y, z, level=0.5) {
  l50 = contourLines(x, y, matrix(z, nrow=length(x), byrow=TRUE), levels = level)[[1]]
  class(l50) = c("l50", class(l50))
  return(l50)
}

selectivityPlot <- function(x, y, z, levels=0.5, lwd=2, ...){
  image.plot(x, y, matrix(z, nrow=length(x), byrow=TRUE), ..., xlab="", ylab="length")
  contour(x, y, matrix(z, nrow=length(x), byrow=TRUE), add=TRUE, levels = levels, lwd=lwd)
  return(invisible())
}

lines.l50 = function(x, ...) lines(x$x, x$y, ...)

print.l50 = function(x, ...) cat("L50 = ", round(mean(x$y), 2), "cm (average).\n")