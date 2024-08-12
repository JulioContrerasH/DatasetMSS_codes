library(sf)
library(mapview)

l5 <- read.csv("data/L5.csv")
l4 <- read.csv("L4.csv")
geom <- read_sf("cloudsen12_metadata.geojson")

# size of the point in R is 
mapview(geom[geom$roi_id %in% names(table(l5$roi_id)),], cex=1) 
mapview(geom[geom$roi_id %in% names(table(l4$roi_id)),], cex=1)
