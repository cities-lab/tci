# Heat map indtruction: http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html
# Load required packages
  library(ggplot2)
  library(gstat)
  library(sp)
  library(maptools)

  load("output/Portland/2011/survey/minutes/tcost.RData")
  tcost.pp <- tcost.hh %>%
                 filter(SAMPN != 8044889) %>% # Eliminate outlier
                 mutate(tcost=log(tcost/HHSIZ)) %>%
                 as.data.frame()

  coordinates(tcost.pp) = ~x + y
  plot(tcost.pp)

  x.range <- as.numeric(c(-123.65, -121.5))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(44.8, 46.12))  # min/max latitude of the interpolation area
  
  # Load districts shape files 
  districtsPoly <- readShapePoly("data/Portland/2011/shp/districts.shp", proj4string=CRS("+init=epsg:2913"))
  shpfile.CRS <- CRS(paste0("+init=epsg:", 4326))
  districtsPoly <- spTransform(districtsPoly, shpfile.CRS)
  districtsPoly <- fortify(districtsPoly, region="DISTRICT")
  
  # expand points to grid
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.02), 
                       y = seq(from = y.range[1], to = y.range[2], by = 0.02))  
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(tcost.pp, pch = 1, col = "red", cex = 0.1)
  
  idw <- idw(formula = tcost ~ 1, locations = tcost.pp, newdata = grd, idp=10)  # apply idw model for the data
  
  idw.output = as.data.frame(idw)  # output is defined as a data table
  names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables
  
  tcost.pp$lon <- tcost.pp$x
  tcost.pp$lat <- tcost.pp$y
  
  heatMap <-  ggplot() + geom_tile(data = idw.output, alpha = 0.8, aes(x = long, y = lat, fill = round(var1.pred, 2))) + 
              scale_fill_gradient(low = "gray100", high = "grey0", breaks = pretty_breaks(n = 10)) + 
              geom_path(data = districtsPoly, aes(long, lat, group = group), colour = "gray30")  + 
              labs(fill = "Travel costs\n(log(minutes))")
  heatMap
            
  output_file = file.path("output/Portland/2011/survey/minutes/heatMap.png")
  ggsave(heatMap, file = output_file, type = "cairo-png")
  
  