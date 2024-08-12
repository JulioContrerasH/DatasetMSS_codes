library(sf)
library(rgee)
library(mapview)

ee_Initialize()

# Load Landsat tile polygons from a geopackage file
landsat_tiles <- read_sf("tiles/TilesWorld.gpkg", layer = "L8L9")

# Load image metadata from a CSV file
images <- read.csv("metadata/metadata.csv")
nrow(images) # 275622

# Get the ID of the first image from the metadata
id <- images$oli_id[150] # 35, 150, 300

# Create an Earth Engine image object using the ID
image <- ee$Image(id)

# Extract the numeric part from the image ID (example: "039015") ^.LC0[8-9](\\d+).$", "\\1

part <- gsub(".*_(\\d+)_.*", "\\1", id)

# Extract the "path" (first three digits) and "row" (next three digits) parts from the number
path <- as.numeric(substr(part, 1, 3))
row <- as.numeric(substr(part, 4, 6))

# Filter the tile polygon corresponding to "path" and "row"
tile <- landsat_tiles[landsat_tiles$PATH == path & landsat_tiles$ROW == row, ]

# Convert the tile polygon to an Earth Engine object
ee_roi <- sf_as_ee(tile)

# Center the map on the Earth Engine image object
Map$centerObject(image)

# Add the image layer to the map and also add the tile polygon layer
print(id)
Map$addLayer(image) +
  Map$addLayer(ee_roi) + 
  Map$addLayer(image$geometry())
