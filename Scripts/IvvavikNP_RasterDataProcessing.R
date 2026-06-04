library(terra)
terraOptions(memfrac = 0.8)
library(tidyverse)
library(tidyterra)
# BELOW: For calculating TWI with GRASS - also good for large raster processing
# library(fasterRaster)
# faster(grassDir = "C:/Program Files/GRASS GIS 8.4")

setwd("C:/Users/jseider.stu")

#dem <- rast("./Sync/Data/ArcticDEM/202505_ArcticDEM_mosiac_2m.tif")
dem_10m <- rast("./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif")

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
twi_10m <- fast(
  "./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"
)$dem_10m %>%
  wetness() %>%
  rast() %>%
  rename(twi_10m = dem_10m_twi)

# Calculate Geomorphons (see: https://doi.org/10.1016/j.geomorph.2012.11.005)
library(rgeomorphon)
Sys.setenv(R_RGEOMORPHON_N_WORKERS = 24)
Sys.setenv(R_RGEOMORPHON_MEM_SCALE_NEED = 5)

rg <- rgeomorphon::geomorphons(
  elevation = dem_10m,
  filename = "C:/Users/jseider.stu/Desktop/geomorphons_0_40.tif",
  forms = "forms10",
  search = 40, # outer search radius (cells)
  skip = 0, # inner skip radius (cells)
  flat_angle_deg = 1 # flat angle threshold (degrees)
)

# MS-LSP (Phenology)
# EVI Area (proxy for growing season length and magnitude)
eviarea <- rast("./Sync/Data/Phenology/MS-LSP/phenology_EVIarea.tif") %>%
  project(ivvavik, method = "bilinear") %>% # note that bilinear interpolation will modify the raw values
  crop(ivvavik, mask = TRUE) %>%
  resample(dem_10m)
names(eviarea) <- c(
  "eviarea_2016",
  "eviarea_2017",
  "eviarea_2018",
  "eviarea_2019",
  "eviarea_2020",
  "eviarea_2021",
  "eviarea_2022",
  "eviarea_2023"
)
writeRaster(eviarea, "./Sync/Data/Phenology/MS-LSP/eviarea_ivvavik.tif")


# Macander PFT Top Cover (2015)
tc_2015 <- list.files(
  "./Sync/Data/AnnualPFT_ABoVE_Macander/2015",
  full.names = TRUE
) %>%
  rast()

ivvavik_pftproj <- vect(
  "./Sync/Data/ParksCanada/IvvavikNationalPark/IvvavikNationalPark.shp"
) %>%
  project(tc_2015)

tc_2015 <- tc_2015 %>%
  crop(ivvavik_pftproj, mask = TRUE) %>%
  project(dem_10m)
names(tc_2015) <- c(
  "broadleaf_tree_2015",
  "conifer_tree_2015",
  "deciduous_shrub_2015",
  "evergreen_shrub_2015",
  "forb_2015",
  "graminoid_2015",
  "lichen_2015"
)
writeRaster(
  tc_2015,
  "./Sync/Data/AnnualPFT_ABoVE_Macander/IvvavikNP_2015_TopCover.tif"
)


## DayMet
# Daylength
dayl <- rast("E:/DayMet/Ivvavik/Ivvavik_daymet_v4_daily_na_dayl_2016.tif") %>%
  project(dem_10m) %>%
  crop(ivvavik_demproj, mask = TRUE)

# Max temperature
max_temp <- list.files(
  "E:/DayMet/Ivvavik/",
  pattern = "*tmax_*",
  full.names = TRUE
) %>%
  rast()
names(max_temp) <- paste0(rep(2016:2023, each = 365), "_", names(max_temp))

mean_tmax <- tapp(
  max_temp,
  index = rep(1:365, times = 8),
  fun = mean,
  na.rm = TRUE
)
names(mean_tmax) <- paste0("avg_tmax_doy_", sprintf("%03d", 1:365))

mean_tmax <- mean_tmax %>%
  project(dem_10m) %>%
  crop(ivvavik_demproj, mask = TRUE)

## Calculate GDD up to day 150 (rough average greenup date)
gdd_doy150 <- mean_tmax[[1:150]]

basetemp <- 0

gdd_doy150[gdd_doy150 < basetemp] <- basetemp
gdd_doy150 <- sum(gdd_doy150 - basetemp, na.rm = TRUE)
names(gdd_doy150) <- "GDD_0C_DOY150"

## Monthly means
# March is approximately DOY 60 to 90 (31 days)
march_tmax <- mean(mean_tmax[[60:90]], na.rm = TRUE)
names(march_tmax) <- "March_Mean_Tmax"

# April is approximately DOY 91 to 120 (30 days)
april_tmax <- mean(mean_tmax[[91:120]], na.rm = TRUE)
names(april_tmax) <- "April_Mean_Tmax"

# May is approximately DOY 121 to 151 (31 days)
may_tmax <- mean(mean_tmax[[121:151]], na.rm = TRUE)
names(may_tmax) <- "May_Mean_Tmax"

# June is approximately DOY 152 to 181 (30 days)
june_tmax <- mean(mean_tmax[[152:181]], na.rm = TRUE)
names(june_tmax) <- "June_Mean_Tmax"
