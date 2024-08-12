# Load required libraries
library(lubridate) # For date and time manipulation
library(rgeeExtra) # Extended functionality for Google Earth Engine
library(dplyr) # Data manipulation and transformation
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling



#' Check if a dataframe is empty.
#'
#' This function checks whether the provided dataframe is empty, i.e., it does not contain any rows.
#'
#' @param df A dataframe. It should be an object of class 'data.frame'.
#'
#' @return A logical value. TRUE if the dataframe is empty (contains no rows), and FALSE otherwise.
#'
#' @examples
#' df_empty <- data.frame()
#' df_non_empty <- data.frame(a = 1:3, b = c('A', 'B', 'C'))
#' check_01(df_empty)
#' # [1] TRUE
#' check_01(df_non_empty)
#' # [1] FALSE
#'
#' @seealso \code{\link{nrow}}
#'
#' @keywords dataframe manipulation check empty
#' @export
check_01 <- function(df) {
  if (nrow(df) == 0) {
    TRUE
  } else {
    FALSE
  }
}



#' Get the projection of a specified band from an image.
#'
#' This function takes an image object and retrieves the projection information for a specified band.
#'
#' @param image An image object representing raster data.
#'   This should be a valid image object that supports the `select()` and `projection()` methods.
#'
#' @return The projection information for the specified band as returned by the `projection()` function.
#'
#' @examples
#' # Assuming 'image' is a valid image object
#' proj_info <- image_proj(image)
#' print(proj_info)
#'
#' @seealso \code{\link{select}}, \code{\link{projection}}
#'
#' @keywords image processing projection band
#' @export
image_proj <- function(image) {
  image$select("B2")$projection()
}



#' Get the coordinates of a point feature in a specified projection.
#'
#' This function takes a point feature object and converts its coordinates to the specified projection.
#'
#' @param point An Earth Engine point feature object.
#'   This should be a valid point feature object representing a geographic point.
#'   It must be compatible with the Earth Engine API and support the `transform()`, `geometry()`, and `coordinates()` methods.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `crs()` method.
#'
#' @return The coordinates of the point feature in the specified projection.
#'
#' @examples
#' # Assuming 'point' and 'proj' are valid objects
#' coords <- point_coordinates(point, proj)
#' print(coords)
#'
#' @seealso \code{\link{ee$Feature}}, \code{\link{transform}}, \code{\link{geometry}}, \code{\link{coordinates}}
#'
#' @keywords point coordinates projection Earth Engine
#' @export
point_coordinates <- function(point, proj) {
  Coords <- ee$Feature(point)$
    transform(proj$crs())$
    geometry()$
    coordinates()
}



#' Initialize coordinate value for image axis using regular expressions.
#'
#' This function initializes a coordinate value for the specified image axis based on the provided projection.
#' The function uses regular expressions to extract the coordinate value from the projection.
#'
#' @param axis A character indicating the axis to initialize. Should be either "x" or "y".
#' 
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `transform()` and `get()` methods.
#'
#' @return A numeric value representing the coordinate value for the specified axis.
#'
#' @examples
#' # Assuming 'axis' is either "x" or "y", and 'proj' is a valid projection object
#' coord_value <- init_coord_image(axis, proj)
#' print(coord_value)
#'
#' @export
init_coord_image <- function(axis, proj) {
  name <- NULL
  if (axis == "x") {
    name <- "elt_0_2"
  } else if (axis == "y") {
    name <- "elt_1_2"
  } else {
    stop("Axis must be 'x' or 'y'")
  }
  ee$Number$parse(
    ee$String(
      proj$transform()$
        match(sprintf('"%s", (-?\\d+\\.?\\d*)', name))$
        get(1)
      )
    )
}

#' Generate a quality mask for cloud-free pixels.
#'
#' This function generates a quality mask for cloud-free pixels based on the 'QA_PIXEL' band in the image.
#'
#' @param image An Earth Engine image object.
#'   This should be a valid image object with the 'QA_PIXEL' band to generate the quality mask.
#'
#' @return An Earth Engine image object representing the quality mask for cloud-free pixels.
#'
#' @export
quality_mask <- function(image) { 
  image$select('QA_PIXEL')$
    bitwiseAnd(strtoi('11000', base = 2))$
    eq(0)
}

#' Transfer coordinates based on initial coordinate and constant.
#'
#' This function transfers the given coordinates based on an initial coordinate and a constant factor.
#'
#' @param coord A numeric vector representing the coordinates to transfer.
#'
#' @param coord_ini A numeric vector representing the initial coordinates.
#' 
#' @param const A numeric constant factor to apply during the coordinate transfer.
#'
#' @return A numeric vector containing the transferred coordinates.
#'
#' @export
coord_transfer <- function(coord, coord_ini, scale) {
  coord_ini$
    add(coord$
          add(coord_ini$multiply(-1))$
          multiply(1 / scale)$
          round()$
          multiply(scale))$
    add(scale / 2)
}



#' Create an Earth Engine point geometry.
#'
#' This function creates an Earth Engine point geometry with the given x and y coordinates.
#'
#' @param x The x-coordinate of the point.
#'
#' @param y The y-coordinate of the point.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'
#' @return An Earth Engine point geometry object.
#'
#' @examples
#' # Assuming 'x' and 'y' are valid numeric coordinates, and 'proj' is a valid projection object
#' point_geometry <- create_point(x, y, proj)
#' print(point_geometry)
#'
#' @export
create_point <- function(x, y, proj) {
  ee$Geometry$Point(list(x, y), proj$crs())
}



#' Create a region of interest (ROI) around a point.
#'
#' This function creates a region of interest (ROI) around the specified point with a given side length.
#'
#' @param point An Earth Engine point geometry representing the center of the ROI.
#'
#' @param side The side length of the ROI.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'
#' @return An Earth Engine geometry object representing the region of interest.
#'
#' @examples
#' # Assuming 'point' is a valid Earth Engine point geometry, 'side' is a valid numeric side length, and 'proj' is a valid projection object
#' roi_geometry <- create_roi(point, side, proj)
#' print(roi_geometry)
#'
#' @export
create_roi <- function(point, side, proj) {
  # Create a buffer around the given point to define the ROI
  roi <- point$buffer(
    distance = side / 2,
    proj = proj$crs())
  
  # Get the bounds of the ROI in the specified projection
  roi_bounds <- roi$bounds(proj = proj$crs())
  
  roi_bounds
}



#' Transform a Point to a New Coordinate System and Scale
#'
#' This function takes a point, a projection, and a scale factor, and transforms the point's coordinates
#' to a new coordinate system and scale. It calculates the new coordinates based on the provided scale factor,
#' and the initial coordinates of the image's x and y axes.
#'
#' @param point An sf point geometry to be transformed.
#' @param proj A character string specifying the projection of the input point.
#' @param scale A numeric value indicating the scale factor for coordinate transformation.
#'
#' @return An sf point geometry with transformed coordinates in the new coordinate system.
#'
#' @examples
#' # Create an sf point geometry
#' point <- st_point(c(100, 200))
#' 
#' # Define a projection and scale
#' proj <- "+proj=utm +zone=10 +datum=WGS84"
#' scale <- 0.5
#' 
#' # Transform the point using the point_tile function
#' transformed_point <- point_tile(point, proj, scale)
#'
#' @importFrom sf st_point
#' @importFrom sf st_transform
#' @import ee
#'
#' @export
point_tile <- function(point, proj, scale) {
  # Calculate the tile coordinates for the given point
  Coords <- point_coordinates(point, proj)
  x <- ee$Number(Coords$get(0))
  y <- ee$Number(Coords$get(1))
  x_ini <- init_coord_image("x", proj)
  y_ini <- init_coord_image("y", proj)
  new_x <- coord_transfer(x, x_ini, scale)
  new_y <- coord_transfer(y, y_ini, scale)
  
  # Create a new point feature with the calculated tile coordinates
  create_point(new_x, new_y, proj)
}



#' Create a Study Area Tile Based on a Point
#'
#' This function creates a study area tile centered around a given point. The tile's size is determined by
#' the provided side length and the point's transformed coordinates. It uses the \code{\link{point_tile}} function
#' to transform the point's coordinates and then generates a rectangular study area tile around the transformed point.
#'
#' @param point An sf point geometry to be used as the center of the study area tile.
#' @param proj A character string specifying the projection of the input point.
#' @param scale A numeric value indicating the scale factor for coordinate transformation.
#' @param side A numeric value indicating the side length of the rectangular study area tile.
#'
#' @return An sf polygon geometry representing the study area tile.
#'
#' @examples
#' # Create an sf point geometry
#' point <- st_point(c(100, 200))
#'
#' # Define a projection, scale, and side length
#' proj <- "+proj=utm +zone=10 +datum=WGS84"
#' scale <- 0.5
#' side <- 500
#'
#' # Generate a study area tile using the tile_study function
#' study_tile <- tile_study(point, proj, scale, side)
#'
#' @importFrom sf st_point
#' @importFrom sf st_polygon
#' @importFrom sf st_union
#'
#' @export
tile_study <- function(point, proj, scale, side) {
  # Convert the point to the corresponding tile coordinates
  ee_new_point <- point_tile(point, proj, scale)
  
  # Create a region of interest (ROI) around the converted point with the specified side length
  create_roi(ee_new_point, side, proj)
}



 



#' Count the number of cloud-free pixels in an image within a region of interest (ROI).
#'
#' This function counts the number of cloud-free pixels in an image within a specified region of interest (ROI).
#'
#' @param image An Earth Engine image object.
#'   This should be a valid image object containing the pixels to count within the ROI.
#'
#' @param roi An Earth Engine geometry object representing the region of interest (ROI).
#'
#' @param scale A numeric value representing the scale at which to perform the reduction operation.
#'
#' @return A numeric value representing the count of cloud-free pixels within the ROI.
#'
#' @export
cloud_free_count <- function(image, roi, scale) {
  # Reduce the image to a single value representing the sum of cloud-free pixels within the ROI
  count_dict <- image$reduceRegion(
    reducer = ee$Reducer$sum(),
    geometry = roi,
    scale = scale
  )
  
  # Return the dictionary containing the count information
  count_dict
}



#' Calculate cloud percentage based on the number of cloud-free pixels.
#'
#' This function calculates the cloud percentage based on the number of cloud-free pixels
#' within a given number of pixels and the scale of analysis.
#'
#' @param pixels A numeric value representing the count of cloud-free pixels.
#'
#' @param scale A numeric value representing the scale of analysis.
#'
#' @param side The side length of the study region.
#'
#' @return A numeric value representing the cloud percentage.
#'
#' @export
cloud_percal <- function(pixels, scale, side) {
  # Calculate the percentage of cloud-free pixels using a formula
  percentage_cloud_free <- ee$Number(1)$
    add(pixels$
          getNumber("QA_PIXEL")$
          multiply(1 / ((side / scale)** 2))$
          multiply(-1))$
    multiply(100)
  
  # Return the calculated percentage
  percentage_cloud_free
}



#' Calculate cloud percentage for a tile around a given point.
#'
#' This function calculates the cloud percentage for a tile around the specified point based on the given scale and side length.
#'
#' @param point An Earth Engine point geometry representing the center of the tile.
#'
#' @param scale A numeric value representing the scale of analysis.
#'
#' @param side The side length of the study region.
#'
#' @return A function that calculates the cloud percentage for the tile when applied to an Earth Engine image.
#'
#' @seealso \code{\link{image_proj}}, \code{\link{tile_study}}, \code{\link{quality_mask}}, \code{\link{cloud_free_count}}, \code{\link{cloud_percal}}
#'
#' @export
cloud_pr_tile <- function(point, scale, side){
  # Define a nested function to process cloud cover information for a single image
  img_cloud_tile <- function(image) {
    
    # Obtain the projection of the image
    projection <- image_proj(image)
    
    # Create a region of interest for the image tile
    ee_roi <- tile_study(point, projection, scale, side)
    
    # Apply cloud masking to the image
    cloud_mask <- quality_mask(image)
    
    # Count the number of cloud-free pixels within the image tile
    count <- cloud_free_count(cloud_mask, ee_roi, scale)
    
    # Calculate the percentage of cloud-free pixels
    prop_cloud <- cloud_percal(count, scale, side)
    
    # Obtain the coordinates of the point in different coordinate systems
    Coords <- point_tile(point, projection, scale)
    Coords_g <- ee$Feature(Coords)$transform()$geometry()$coordinates()
    
    # Set properties on the image
    image$set(list("cloud_tile" = prop_cloud))$
      set(list("X" = ee$Number(Coords_g$get(0))))$
      set(list("Y" = ee$Number(Coords_g$get(1))))$
      set(list("E" = ee$Number(Coords$coordinates()$get(0))))$
      set(list("N" = ee$Number(Coords$coordinates()$get(1))))$
      set(list("CRS" = projection$crs()))$
      set(list("PR" = ee$Number$format(image$get("WRS_PATH"), '%.0f')$
                 cat(ee$Number$format(image$get("WRS_ROW"), '%.0f'))))
  }
  
  # Return the nested function for processing cloud cover information
  img_cloud_tile
}



#' Add PR Tile Information to Image Metadata
#'
#' This function adds PR (Path-Row) tile information to the metadata of an Earth Engine image.
#'
#' @param image An Earth Engine image object.
#'
#' @return An Earth Engine image object with added PR (Path-Row) tile information.
#'
#' @examples
#' \dontrun{
#' image <- ee$Image("your/image/asset")
#' image_with_pr <- pr_tile(image)
#' }
#'
#' @export
pr_tile <- function(image) {
  # Extract the WRS_PATH and WRS_ROW properties from the image
  path <- ee$Number$format(image$get("WRS_PATH"), '%.0f')
  row <- ee$Number$format(image$get("WRS_ROW"), '%.0f')
  
  # Concatenate the path and row values to create the "PR" property
  image$set(list("PR" = path$cat(row)))
}



#' Filter image collection by geographic bounds around a point.
#'
#' This function filters an Earth Engine image collection to include only images that intersect the geographic bounds around a specified point.
#'
#' @param snippet An Earth Engine image collection snippet.
#'
#' @param point An Earth Engine point geometry representing the center of the geographic bounds.
#'
#' @return An Earth Engine image collection filtered by geographic bounds around the point.
#'
#' @export
images_bounds <- function(snippet, point) {
  # Filter the Image Collection using the provided point's bounds
  filtered_ic <- ee$ImageCollection(snippet) %>%
    ee$ImageCollection$filterBounds(point)
  
  filtered_ic
}



#' Extract and merge date information from a list of image collections
#'
#' This function takes a list of image collections, extracts date information from each
#' collection, and then merges them into a single image collection with date metadata.
#'
#' @param listing A list of image collections to be merged.
#'
#' @return An image collection with merged date metadata.
#'
#' @examples
#' id_dates_ic(list_of_ic)
#'
#' @export
id_dates_ic <- function(listing) {
  merged_ic <- NA
  for (i in 1:length(listing)) {
    if (i == 1) {
      merged_ic <- listing[[i]]
    } else {
      merged_ic <- merged_ic$merge(listing[[i]])
    }
  }
  
  # Call a custom function to extract and set date information.
  merged_ic %>% ee_get_date_ic()
}



#' Filter image dates with a specific pattern in their IDs.
#'
#' This function filters the input image dates to include only those with a specific pattern in their IDs.
#'
#' @param idates An Earth Engine image collection containing image dates with IDs.
#'
#' @param pattern A character pattern to match in the IDs of the image dates.
#'
#' @return An Earth Engine image collection filtered based on the specified pattern in the IDs.
#'
#' @export
sensor_pattern <- function(idates, pattern) {
  IDateS <- idates[grepl(pattern, idates$id),]
  return(IDateS)
}



#' Calculate time difference and index between two sensor time series
#'
#' This function calculates the time difference and index between two sensor time series,
#' returning a data frame with the time difference and the corresponding index of the closest time point
#' in the second sensor time series for each time point in the first sensor time series.
#'
#' @param sensor1 A data frame representing the first sensor time series.
#' @param sensor2 A data frame representing the second sensor time series.
#' @param units The time units for the difference calculation.
#'
#' @return A data frame with the calculated time differences and corresponding indices.
#'
#' @examples
#' time_between(sensor1, sensor2, "hours")
#'
#' @export
time_between <- function(sensor1, sensor2, units) {
  
  # Calculate time difference and index
  r_collocation <- sapply(
    X = sensor1$time_start,
    FUN = function(x) {
      vresults <- abs(as.numeric(sensor2$time_start - x, units = units))
      c(min(vresults), which.min(vresults))
    }
  )
  
  # Create data frame
  result_df <- data.frame(time = r_collocation[1, ], 
                          index = r_collocation[2, ])
  
  return(result_df)
}



#' Get Satellite Metadata
#'
#' Retrieves metadata from different image collections based on specified parameters.
#'
#' @param timediff The time difference in seconds used for image filtering.
#' @param point The spatial point used as the filter bounds.
#' @return A data frame containing the retrieved metadata, including MSS and TM ID, Tier, ROI ID, and time difference.
get_metadata <- function(point, snip1, snip2, sensors, units, scale, side, timediff, max_ob) { 

  # Generation of dates and id by image
  ee_point <- sf_as_ee(point$geometry)
  
  # Apply the 'images_bounds' function to each element in 'snip1' and 'snip2'
  listing1 <- lapply(snip1, images_bounds, ee_point)
  listing2 <- lapply(snip2, images_bounds, ee_point)
  
  # Convert lists of geometries to image collections
  imgstm <- ee$ImageCollection(ee$FeatureCollection(listing1)$flatten())
  imgsmss <- ee$ImageCollection(ee$FeatureCollection(listing2)$flatten())

  # Apply the 'cloud_tile' function to each image in 'imgsmsi' and 'CloudImgs'
  imgstm_filter <- imgstm$map(cloud_pr_tile(ee_point, scale, side)) %>%
    ee$ImageCollection$filterMetadata("cloud_tile", "less_than", 15) #%>%
    # ee$ImageCollection$filter(ee$Filter$inList("PR", ee$List(strsplit(point$PR, split = "-"))))
  
  imgsmss_filter <- imgsmss$map(pr_tile) #%>%
    # ee$ImageCollection$filter(ee$Filter$inList("PR", ee$List(strsplit(point$PR, split = "-"))))

  # Combine the 'CloudImgsFilter' and 'imgsmss' image collections
  all_together_db <- id_dates_ic(list(imgstm_filter, imgsmss_filter))
  
  # Filter First Sensor ID
  sensor1 <- sensor_pattern(all_together_db, sensors[1])
  
  # Check if there is any image in the collection
  if (check_01(sensor1)) {
    return(NA)
  }
  
  # Filter Second Sensor ID
  sensor2 <- sensor_pattern(all_together_db, sensors[2])
  
  # Check if there is any image in the collection
  if (check_01(sensor2)) {
    return(NA)
  }
  
  # Get time interval between images from different sensors
  Tindex <- time_between(sensor1, sensor2, units)
  difft <-  Tindex$time < timediff
  
  # Check if the images are on the time interval 
  if (sum(difft) == 0) {
    return(NA)
  }

  # Filter IDs of each sensor in set time condition
  final_tm <- sensor1[difft, ]
  final_time <- Tindex$time[difft]
  final_mss <- sensor2[Tindex$index[difft], ]
  
  # Characteristics of the first sensor
  Sat_tm <- substr(final_tm$id, 9, 12)
  Mission_tm <- paste0(substr(Sat_tm, 1, 1), substr(Sat_tm, nchar(Sat_tm), nchar(Sat_tm)))
  Tier_tm <- substr(final_tm$id, 18, 19)
  
  # Second Sensor Characteristics
  Sat_mss <- substr(final_mss$id, 9, 12)
  Mission_mss <- paste0(substr(Sat_mss, 1, 1), substr(Sat_mss, nchar(Sat_mss), nchar(Sat_mss)))
  Tier_mss <- substr(final_mss$id, 18, 19)

  # Generation of metadata table and characteristics
  df <- data.frame(
    mss_id = final_mss$id,
    Mission_mss = Mission_mss,
    Tier_mss = Tier_mss,
    tm_id = final_tm$id,
    Mission_tm = Mission_tm,
    Tier_tm = Tier_tm,
    dif_time = round(final_time, 10),
    roi_id = point$roi_id
  )
  
  df <- df[order(df$dif_time), ]
  
  if(max_ob) {
    n <- min(max_ob, nrow(df)) 
    df <- df[1:n, ]
  }

  # Generation of final metadata table and characteristics
  Imgv <- c()
  for(i in 1:length(df$tm_id)) {
    Imgv <- c(Imgv, ee$Image(df$tm_id[i]))
  }
  Imgs <- ee$ImageCollection(Imgv)
  CloudImgs <- Imgs$map(cloud_pr_tile(ee_point, scale, side))

  df_meta <- data.frame(
    ee$Dictionary$
      fromLists(
        c("cloud_tile", "X", "Y", "E", "N", 
          "CRS", "sun_azimuth", "sun_elevation"), 
        c(CloudImgs$aggregate_array("cloud_tile"), 
          CloudImgs$aggregate_array("X"),
          CloudImgs$aggregate_array("Y"),
          CloudImgs$aggregate_array("E"),
          CloudImgs$aggregate_array("N"),
          CloudImgs$aggregate_array("CRS"),
          Imgs$aggregate_array("SUN_AZIMUTH"),
          Imgs$aggregate_array("SUN_ELEVATION")))$getInfo()
  )
  return(cbind(df, df_meta))
}



#' Convert Digital Number (DN) values to Top-of-Atmosphere (TOA) reflectance for Landsat MSS imagery.
#'
#' This function converts Landsat MSS imagery from Digital Number (DN) values to Top-of-Atmosphere (TOA) reflectance.
#'
#' @param img An Earth Engine image object representing the Landsat MSS imagery.
#'   This should be a valid image object with bands "B1", "B2", "B3", "B4", and "QA_PIXEL".
#'
#' @return An Earth Engine image object with TOA reflectance bands "B1", "B2", "B3", "B4", and "QA_PIXEL".
#'
#' @examples
#' # Assuming 'img' is a valid Earth Engine image object
#' toa_img <- MSS_TOA(img)
#' print(toa_img)
#'
#' @export
MSS_TOA <- function(img) {
  
  # Select DN bands and QA_PIXEL band
  dnImg <- img$select(c("B1", "B2", "B3", "B4"))
  Qa <- img$select(c("QA_PIXEL"))
  
  # Get gain and bias bands
  gainBands <- ee$List(img$propertyNames())$
    filter(ee$Filter$stringContains("item", "REFLECTANCE_MULT_BAND"))$
    sort()
  biasBands <- ee$List(img$propertyNames())$
    filter(ee$Filter$stringContains("item", "REFLECTANCE_ADD_BAND"))$
    sort()
  
  # Create gain and bias images
  gainImg <- ee$Image$cat(
    ee$Image$constant(img$get(gainBands$get(0))),
    ee$Image$constant(img$get(gainBands$get(1))),
    ee$Image$constant(img$get(gainBands$get(2))),
    ee$Image$constant(img$get(gainBands$get(3)))
  )
  biasImg <- ee$Image$cat(
    ee$Image$constant(img$get(biasBands$get(0))),
    ee$Image$constant(img$get(biasBands$get(1))),
    ee$Image$constant(img$get(biasBands$get(2))),
    ee$Image$constant(img$get(biasBands$get(3)))
  )
  
  # Apply conversion and add QA_PIXEL band
  dnImg$multiply(gainImg)$
    add(biasImg)$
    addBands(Qa)$
    rename(c("B1", "B2", "B3", "B4", "QA_PIXEL"))
}



#' Download function for image pairs
#'
#' This function downloads image pairs based on the given row data, sensor names, region of interest, and output directory.
#'
#' @param row A data frame row containing image IDs and coordinates.
#' @param sensors A character vector specifying the sensor names for the image pair (e.g., c("TM", "MSS")).
#' @param side A numeric value specifying the buffer side length for the region of interest.
#' @param output A character string specifying the output directory for downloaded images.
#'
#' @return The function downloads and saves image pairs based on the specified parameters.
#'
#' @importFrom sf st_as_sf st_transform
#' @importFrom reticulate reprex
#' @importFrom sf sf_as_ee
#'
#' @examples
#' download(row, c("TM", "MSS"), 30, "output_dir")
#'
#' @export
download <- function(row, sensors, side, output) {
  
  # Define options for image approach
  options <- list(LT = c("tm_id", 30), LM = c("mss_id", 60), MSI = c("msi_id", 10), OLI = c("oli_id", 30))
  
  # Create output directories if they don't exist
  dir.create(sprintf("%s/%s", output, sensors[1]), 
             showWarnings = FALSE, 
             recursive = TRUE)
  dir.create(sprintf("%s/%s", output, sensors[2]), 
             showWarnings = FALSE, 
             recursive = TRUE)
  
  # Set scaling factor based on options
  scale <- as.numeric(options[[sensors[1]]][2])
  
  # Convert coordinates and get image IDs
  point <- st_as_sf(x = row, coords = c("X", "Y"), crs = 4326)
  img1 <- as.character(row[options[[sensors[1]]][1]])
  img2 <- as.character(row[options[[sensors[2]]][1]])
  
  # Prepare image data
  ImageC1 <- ee$Image(img1)$
    select(c("B2", "B3", "B4"))$ 
    multiply(10000)$
    int16()$
    unmask(-99, sameFootprint = FALSE)
  
  ImageC2 <- MSS_TOA(ee$Image(img2))$ 
    select(c("B1", "B2", "B3", "B4"))$
    multiply(10000)$
    int16()$
    unmask(-99, sameFootprint = FALSE)
  
  # Obtain projection metadata
  proj_metadata <- ImageC1$select("B2")$projection()$getInfo()
  proj_crs <- proj_metadata$crs
  
  # Transform point to UTM and reproject ImageC2
  geom <- point$geometry
  geom_utm <- st_transform(geom, proj_crs)
  ImageC2_crs <- ImageC2$reproject(proj_crs) 
  
  # Create region of interest
  roi <- geom_utm %>%
    st_buffer(side / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)
  
  # Download and save images
  output_file1 <- sprintf("%s/%s/%s__%s.tif", output, sensors[1], row$roi_id, basename(img1))
  if (!file.exists(output_file1)) {
    lr_image <- ee_as_rast(
      image = ImageC1,
      region = ee_roi,
      scale = scale,
      dsn = output_file1
    )
  }
  
  output_file2 <- sprintf("%s/%s/%s__%s.tif", output, sensors[2], row$roi_id, basename(img2))
  if (!file.exists(output_file2)) {
    hr_image <- ee_as_rast(
      image = ImageC2_crs,
      region = ee_roi,
      scale = scale,
      dsn = output_file2
    )
  }
}



#' Display function for image comparison
#'
#' This function displays image comparisons based on the given row data.
#'
#' @param row A data frame row containing image IDs, time difference, and coordinates.
#' @param mode A character specifying the display mode ("points" or "comparison").
#' @param side A numeric value specifying the buffer side length for the region of interest.
#' @param max A numeric value specifying the maximum pixel value for visualization.
#'
#' @return The function displays image comparisons on the map.
#'
#' @importFrom sf st_as_sf st_transform
#' @importFrom mapview mapview
#' @importFrom leaflet leaflet
#' @importFrom leafem sf_as_ee
#' @importFrom googleAuthR googleAuthR::gar_auth
#' @importFrom reticulate reprex
#' @importFrom purrr map
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot geom_sf
#'
#' @examples
#' display(row, "points", 30, 0.3)
#'
#' @export
display <- function(row, mode, side, max = 0.3) {
  
  # Extract relevant data from the row
  img_tm <-  ee$Image(row$tm_id)
  proj_metadata <- img_tm$select("B2")$projection()$getInfo()
  proj_transform <- proj_metadata$transform
  proj_crs <- proj_metadata$crs
  
  # Convert coordinates and create buffer
  point <- st_as_sf(x = row, coords = c("X", "Y"), crs = 4326)
  geom_utm <- st_transform(point$geometry, proj_crs)
  ee_point <- sf_as_ee(geom_utm)
  roi <- geom_utm %>% st_buffer(side / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)
  
  # Prepare image data
  eeimg1 <- img_tm$clip(ee_roi)
  eeimg2 <- MSS_TOA(ee$Image(row$mss_id))$reproject(proj_crs)$clip(ee_roi)
  
  # Define visualization settings
  eel1 <- list(min = 0, 
               max = max, 
               bands = c("B4", "B3", "B2"))
  
  eel2 <- list(min = 0, 
               max = max / 2, 
               bands = c("B4", "B3", "B2"))
  
  # Print information
  print(sprintf("Diferencia de %s minutos entre MSS: %s y TM: %s", 
                row$dif_time, 
                row$mss_id, 
                row$tm_id))
  
  # Center the map
  Map$centerObject(eeimg1)
  
  # Display based on mode
  if (mode == "points") {
    Map$addLayer(eeimg1, eel1, basename(row$tm_id)) +
      Map$addLayer(eeimg2, eel2, basename(row$mss_id)) +
      Map$addLayer(ee_point, list(color = "#E4FF00"), "Centered point (yellow)")
  } else if (mode == "comparison") {
    Map$addLayer(eeimg1, eel1, basename(row$tm_id)) | 
      Map$addLayer(eeimg2, eel2, basename(row$mss_id))
  } else {
    stop("Modo no válido. Debe ser 'points' o 'comparison'.")
  }
}



#' Get metadata for a given point and sensor, with retry mechanism on failure.
#'
#' This function retrieves metadata for a specific point and sensor using the `get_metadata` function. If an error occurs
#' during the metadata retrieval, the function will retry up to four additional times (by default) before raising a
#' custom error message indicating possible internet connection issues.
#'
#' @param point A spatial point object representing the location of interest.
#' @param sensor A character string specifying the sensor for which metadata is requested.
#' @param timediff The time difference to consider when fetching the metadata.
#' @param counter An optional integer specifying the retry counter. Defaults to 1.
#' @return The metadata retrieved using the `get_metadata` function, or an error message if retries fail.
get_metadata_try <- function(point, snip1, snip2, sensors, units, scale, side, timediff = 20, max_ob = F, counter = 1) {
  results <- try(
    get_metadata(point = point, snip1 = snip1, snip2 = snip2, sensors = sensors, units = units, scale = scale, side = side, timediff = timediff, max_ob = max_ob)
    )
  if (inherits(class(results), "try-error")) {
    counter <- counter + 1
    if (counter == 2) stop("Probably internet connection lost")
    get_metadata_try(point, snip1, snip2, sensors, units, scale, side, timediff = 20, max_ob = F, counter = 1)
  }

  results
}