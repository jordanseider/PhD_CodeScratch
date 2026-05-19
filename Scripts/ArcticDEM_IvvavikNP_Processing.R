library(terra)
terraOptions(memfrac = 0.8)
library(tidyverse)
library(tidyterra)
# BELOW: For calculating TWI with GRASS - also good for large raster processing
library(fasterRaster)
faster(grassDir = "C:/Program Files/GRASS GIS 8.4")

setwd("C:/Users/jseider.stu")

dem <- rast("./Sync/Data/ArcticDEM/202505_ArcticDEM_mosiac_2m.tif")

ivvavik_demproj <- vect(
  "./Sync/Data/ParksCanada/IvvavikNationalPark/IvvavikNationalPark.shp"
) %>%
  project(dem)

ivvavik <- vect(
  "./Sync/Data/ParksCanada/IvvavikNationalPark/IvvavikNationalPark.shp"
) %>%
  project("EPSG:32607") # WSG 84/UTM 7N

# 2-metre raster
dem <- dem %>%
  crop(
    ivvavik_demproj,
    mask = TRUE
  ) %>%
  project(
    ivvavik, # WSG 84/UTM 7N
    threads = TRUE
  ) %>%
  rename(
    dem_2m = dem,
    slope_2m = slope,
    aspect_2m = aspect,
    TPI_2m = TPI
  )

dem$aspect_ns_2m <- cos((dem$aspect_2m * pi) / 180)
dem$aspect_ew_2m <- sin((dem$aspect_2m * pi) / 180)
gc()

writeRaster(dem, "./Documents/_R_Working/ArcticDEM_IvvavikNP_2m.tif")

# 10-metre raster
dem_10m <- rast("./Sync/Data/ArcticDEM/202505_ArcticDEM_mosiac_10m.tif")[[
  "dem"
]] %>%
  project(ivvavik) %>%
  crop(ivvavik, mask = TRUE) %>%
  rename(dem_10m = dem)

terrain_10m <- terrain(
  dem_10m,
  c("slope", "aspect", "TPI"),
  unit = "degrees"
) %>%
  rename(
    slope_10m = slope,
    aspect_10m = aspect,
    TPI_10m = TPI
  )

# Create a blank template using the 2m raster's exact spatial extent and origin
template_10m <- rast(dem)
# Override the template's resolution to 10 meters
res(template_10m) <- 10
# Stack 10m layers and resample them into the aligned 10m template
dem_10m <- c(dem_10m, terrain_10m) %>%
  resample(
    template_10m,
    method = "bilinear",
    threads = TRUE
  )

rm(terrain_10m, template_10m)

# Convert aspect to northness
dem_10m$aspect_ns_10m <- cos((dem_10m$aspect_10m * pi) / 180)
dem_10m$aspect_ew_10m <- sin((dem_10m$aspect_10m * pi) / 180)
gc()

writeRaster(
  dem_10m,
  "./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"
)

# Calculate Topographic Wetness Index with GRASS
fastdem <- fast(
  "./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"
)$dem_10m
fastdem <- fast(dem_10m$dem_10m)

twi_10m <- wetness(fastdem)

morph <- geomorphons(
  fastdem,
  inner = 2,
  outer = 20
)
