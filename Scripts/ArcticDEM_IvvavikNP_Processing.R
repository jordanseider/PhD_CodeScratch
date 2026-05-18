library(terra)
terraOptions(memfrac = 0.8)
library(tidyverse)
library(tidyterra)

setwd("C:/Users/jseider.stu")

dem <- rast("./Documents/_R_Working/202505_ArcticDEM_mosiac_2m.tif")

ivvavik_demproj <- vect("./Documents/_R_Working/IvvavikNationalPark.shp") %>%
  project(dem)

ivvavik <- vect("./Documents/_R_Working/IvvavikNationalPark.shp") %>%
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
gc()
dem$aspect_ew_2m <- sin((dem$aspect_2m * pi) / 180)
gc()

# 10-metre raster
dem_10m <- rast("./Sync/Data/ArcticDEM/202505_ArcticDEM_mosiac_10m.tif")[[
  "dem"
]] %>%
  project(ivvavik) %>%
  crop(ivvavik, mask = TRUE) %>%
  terrain(dem_10m, c("slope", "aspect", "TPI"), unit = "degrees") %>%
  rename(
    dem_10m = dem,
    slope_10m = slope,
    aspect_10m = aspect,
    TPI_10m = TPI
  ) %>%
  resample(dem, method = "near")

dem <- c(dem, dem_10m)
rm(dem_10m)

# Convert aspect to northness
dem$aspect_ns_10m <- cos((dem$aspect_10m * pi) / 180)
dem$aspect_ew_10m <- sin((dem$aspect_10m * pi) / 180)

writeRaster(dem, "./Documents/_R_Working/Ivvavik_DEM_UTM7N.tif")
