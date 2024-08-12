# Load the rgee and sf packages for Earth Engine integration and spatial operations.
library(rgee)
library(sf)

# Initialize the Earth Engine API session.
ee_Initialize()



#' Transform Coordinates Using Earth Engine and Spatial Libraries
#'
#' This function transforms the input point geometry to new coordinates using Earth Engine
#' and the `sf` package. It filters an image collection based on the location of the point
#' and performs UTM transformations to generate new coordinates.
#'
#' @param point A single point geometry (sf object) to be transformed.
#' @return A data frame containing the transformed coordinates in both geographic and UTM systems,
#'         as well as metadata about the transformation.
#'
#' @importFrom sf st_transform st_point st_sfc
#' @importFrom mapview sf_as_ee
#' @import ee 
#' @examples
#' point <- st_point(c(10, 20))
#' transformed <- get_trans_coords(point)
#' print(transformed)
#'
get_trans_coords <- function(point) {
  
  # Extract the geometry from the point
  geom <- point$geometry
  
  # Convert the geometry into an Earth Engine object
  ee_point <- geom %>% sf_as_ee()
  
  # Filter the image collection based on location and tile
  ee_ic_ref <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>%
    ee$ImageCollection$filterBounds(ee_point) %>%
    ee$ImageCollection$first()
  
  # Get projection metadata
  proj_metadata <- ee_ic_ref$select("B2")$projection()$getInfo()
  proj_transform <- proj_metadata$transform
  proj_crs <- proj_metadata$crs
  
  # Transform the point geometry to UTM
  geom_utm <- geom %>% st_transform(crs = proj_crs)
  x_utm <- geom_utm[[1]][1]
  y_utm <- geom_utm[[1]][2]
  
  # Calculate new transformed UTM coordinates
  new_x_utm <- proj_transform[3] + round((x_utm - proj_transform[3]) / 10) * 10 + 10 / 2
  new_y_utm <- proj_transform[6] + round((y_utm - proj_transform[6]) / 10) * 10 + 10 / 2
  
  # Create a new transformed UTM geometry
  new_geom_utm <- st_point(c(new_x_utm, new_y_utm)) %>% st_sfc(crs = proj_crs)
  
  # Transform the new geometry to geographic coordinates
  new_point <- new_geom_utm %>% st_transform(crs = 4326)
  new_x_geo <- new_point[[1]][1]
  new_y_geo <- new_point[[1]][2]
  
  # Create a data frame with the results and metadata
  data.frame(
    X = new_x_geo,
    Y = new_y_geo,
    E = new_x_utm,
    N = new_y_utm,
    CRS = proj_crs
  )
}



# Read spatial data from a GeoJSON file into the 'points' object.
points <- read_sf("D:/CURSOS_2022/Repos/AndesDataCube/Dataset/MSI_OLI/tiles/s2landsatpairs.geojson")

# Iterate through each row (point) in the 'points' object.
container <- list()
for (i in 1:nrow(points)) {
  print(i)
  container[[i]] <- get_trans_coords(points[i, ])
}

# Combine the transformed coordinates into 'id_metadata'.
id_metadata <- do.call(rbind, container)

# Combine the 'id_metadata' with the original data, dropping the geometry column.
df <- cbind(id_metadata, st_drop_geometry(points))

# Create a new spatial points data frame and transform its coordinates.
points_c <- st_as_sf(df, coords = c("x", "Y"), crs = 4326)

# Write the transformed points to a new file (provide the desired file path).
st_write(points_c, "")
