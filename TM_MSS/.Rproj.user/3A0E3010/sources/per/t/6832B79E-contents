# /*
# MIT License
#
# Copyright (c) [2023] [AndesDataCube team]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.



# Load required libraries -------------------------------------------------
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling
library(mapview)



# Load the utility functions from 'utils.R' file --------------------------
source("utils.R")



# Initialize EE -----------------------------------------------------------
ee_Initialize()



# Load initial dataset ----------------------------------------------------
metadata <- read_sf("tiles/points.geojson")



# Snippets for sensors and level ------------------------------------------
snip1 <- list(
    "LANDSAT/LT04/C02/T1_TOA",
    "LANDSAT/LT04/C02/T2_TOA",
    "LANDSAT/LT05/C02/T1_TOA",
    "LANDSAT/LT05/C02/T2_TOA"
)

snip2 <- list(
    "LANDSAT/LM04/C02/T1",
    "LANDSAT/LM04/C02/T2",
    "LANDSAT/LM05/C02/T1",
    "LANDSAT/LM05/C02/T2"
)



# Create metadata table, difference 10 seconds ----------------------------
container <- list()
for (index in 1:nrow(metadata)) {

    # Print the index value
    print(index)

    # Get the coordinate data for the current row
    coordinate <- metadata[index, ]

    # Get metadata for satellite images
    img_metadata <- get_metadata_try(
        point = coordinate,
        sensors = c("LT", "LM"),
        snip1 = snip1,
        snip2 = snip2,
        units = "mins",
        scale = 30,
        side = 34560,
        timediff = 15,
        max_ob = 30
    )
    container[[index]] <- img_metadata
}s

id_metadata <- do.call(rbind, container)
id_metadata <- id_metadata[!is.na(id_metadata$mss_id),]
write.csv(id_metadata,"exports/metadata.csv")

# Display -----------------------------------------------------------------
df <- read.csv("exports/metadata.csv") # previo filtro
row <- df[2, ]
display(row = row, 
        mode = "points", # comparison
        side = 34560,
        max = 0.7)



# Download satellite images -----------------------------------------------
df <- read.csv("exports/metadata.csv") 
for (x in 1:2) {
    row = df[x, ]
    download(
        row = row,
        sensors = c("LT", "LM"),
        side = 30720,
        output = "results"
    )
}




# Display points ----------------------------------------------------------

# Read geospatial data 
puntos_ini <- read_sf("data/points.geojson")
puntos_fin <- read.csv("exports/metadata.csv")

# Combine columns into a single string and make them unique
points_fin <- unique(paste(puntos_fin$roi_id, puntos_fin$X, puntos_fin$Y, sep = ", "))
ptfindf <- as.data.frame(do.call(rbind, strsplit(points_fin, split = ", ")))

# Set column names for the data frame to "id", "X", and "Y"
colnames(ptfindf) <- c("id", "X", "Y")

# Convert the "Y" and "X" columns in the data frame to numeric
ptfindf$Y <- as.numeric(ptfindf$Y)
ptfindf$X <- as.numeric(ptfindf$X)

# Create a Simple Features (sf) object from the data frame, specifying coordinates and CRS
puntos_fin <- st_as_sf(ptfindf, coords = c("X", "Y"), crs = 4326)

# Display 
mapview(puntos_ini, col.regions = "green") +  # 10277 points
    mapview(puntos_fin, col.regions = "orange") # 6284 points
