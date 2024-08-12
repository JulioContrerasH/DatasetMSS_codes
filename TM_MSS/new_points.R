# Load required libraries for working with spatial data
library(sf)  # Spatial data manipulation
library(rnaturalearth)  # Access to Natural Earth datasets
library(mapview)  # Interactive mapping

# Set Sentinel-2 data usage to FALSE (not directly relevant to this code block)
sf_use_s2(FALSE)

# Fetch world country boundaries from Natural Earth and remove Antarctica
world <- ne_countries(scale = 110, returnclass = "sf")
world <- world[world$name != "Antarctica",]

# Read Landsat tiles from a geopackage file
wrs_tiles <- read_sf("tiles/TilesWorld.gpkg", layer = "L8L9")

# Select Landsat tiles that intersect with the world boundaries
landsat_tiles <- wrs_tiles[st_intersects(wrs_tiles, st_union(world), sparse = FALSE)[, 1], ]

# Read cloud-sensing points from a geojson file
points <- st_read("tiles/cloudsen12_metadata.geojson")

# Determine which Landsat tiles intersect with the cloud-sensing points
ix <- st_intersects(landsat_tiles, st_union(points), sparse = FALSE)[, 1]
tiles <- landsat_tiles[!ix, ]  # Tiles without intersections
notiles <- landsat_tiles[ix, ]  # Tiles with intersections

# Create a unified geometry of the tiles without intersections
utiles <- st_union(tiles)

# Set random seed for reproducibility
set.seed(100)

# Sample points from the unified tile geometry
ptsa <- st_sample(utiles, size = 6000, min_distance = 30000) %>%
  st_as_sf()

# Select new points that intersect with the world boundaries
new_points <- ptsa[st_intersects(ptsa, st_union(world), sparse = FALSE)[, 1], ]

# Assign ROI IDs to the new points
new_points$roi_id <- paste0("ROI_", 13001:(13000 + nrow(new_points)))

# Write the spatial points data frame to a file (path not provided, leading to an empty filename).
# st_write(sf_points, "")

# Create a map with three layers using the 'mapview' package.
mapview(notiles, col.regions = "lightblue", col = "black", lwd = 1, alpha.regions = 1) +
  mapview(tiles, col.regions = "red", col = "red", lwd = 1, alpha.regions = 0.3) +
  mapview(new_points, col.regions = "black") 