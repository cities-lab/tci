# Heat map indtruction: http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html
# Load required packages
  library(ggplot2)
  library(gstat)
  library(sp)
  library(maptools)

  load("output/Portland/2011/survey/minutes/tcost.RData")
  tcost.hh.test <- tcost.hh

  tcost.hh.test <- as.data.frame(tcost.hh.test)
  coordinates(tcost.hh.test) = ~x + y
  plot(tcost.hh.test)

  x.range <- as.numeric(c(-123.65, -121.5))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(44.8, 46.12))  # min/max latitude of the interpolation area
  
  districtsPoly <- readShapePoly("data/Portland/2011/shp/districts.shp", proj4string=CRS("+init=epsg:2913"))
  shpfile.CRS <- CRS(paste0("+init=epsg:", 4326))
  districtsPoly <- spTransform(districtsPoly, shpfile.CRS)
  districtsPoly <- fortify(districtsPoly, region="DISTRICT")
  
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.005), 
                     y = seq(from = y.range[1], to = y.range[2], by = 0.005))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(tcost.hh.test, pch = 1, col = "red", cex = 0.1)
  
  summary(tcost.hh.test@data$tcost)
  head(tcost.hh.test@data$tcost)
  head(tcost.hh.test@data)
  str(tcost.hh.test)
  idw <- idw(formula = tcost ~ 1, locations = tcost.hh.test, newdata = grd, idp=10)  # apply idw model for the data
  
  idw.output = as.data.frame(idw)  # output is defined as a data table
  head(idw.output)
  names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables
  
  tcost.hh$lon <- tcost.hh$x
  tcost.hh$lat <- tcost.hh$y
  
#   ggplot() + geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) + 
#     geom_point(data = tcost.hh, aes(x = lon, y = lat), shape = 21, 
#                colour = "red", cex=0.1)
  
  
  ggplot() + geom_tile(data = idw.output, alpha = 0.8, aes(x = long, y = lat, fill = round(var1.pred, 2))) + 
    scale_fill_gradient(low = "grey95", high = "grey10", breaks = pretty_breaks(n = 10)) + 
    geom_path(data = districtsPoly, aes(long, lat, group = group), colour = "grey50") + 
    geom_point(data = tcost.hh, aes(x = lon, y = lat), shape = 21, 
               colour = "grey30", cex=0.1)   + labs(fill = "Travel costs\n(minutes)")
  
  
  
  