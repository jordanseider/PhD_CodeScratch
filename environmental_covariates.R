## Continuous spatial data for phenology map

library(terra)
library(dplyr)
library(sf)

setwd("C:/Users/jseider.stu/Sync")

# Load phenocam data
pheno <- read.csv("./Data/Phenology/QHI_Phenocam_Locations.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3579) %>%
  # change the 'name' data to remove "_PHENOCAM" from data
  mutate(name = gsub("_PHENOCAM", "", name))

# Load DEM without TPI layer (poor data for QHI)
covar <- rast("./Data/Phenology/QHI_ArcticDEM_2m.tif") %>%
  subset("TPI", negate = TRUE) %>%
  project("EPSG:3579", threads = 20)

# Convert aspect to northness
covar$aspect_ns <- cos((covar$aspect * pi) / 180)
covar$aspect_ew <- sin((covar$aspect * pi) / 180)

# Calculate Solar Radiation Aspect Index
covar$srai <- (1 - cos((pi / 180) * (covar$aspect - 30))) / 2

# # Topographic Wetness Index
# library(fasterRaster)
# grassDir <- "C:/Program Files/GRASS GIS 8.4"
# faster(grassDir = grassDir)
# 
# dem.GRaster <- fast(covar$dem)
# qhi.wetness <- wetness(dem.GRaster)
# covar$twi <- rast(qhi.wetness)
# 
# writeRaster(qhi.wetness, "./Data/Phenology/qhi_wetness.tif")

covar$twi <- rast("./Data/Phenology/qhi_wetness.tif")

# ABoVE Land Cover
covar$landcover <- rast("./Data/AnnualLandcover_ABoVE_Wang/landcover_2014.tif") %>%
  project(y = covar, 
          method = "near", 
          threads = 20) %>%
  as.factor()

levels(covar$landcover) <- data.frame(ID = c(1:15),
                                      cat = c("Evergreen Forest",
                                              "Deciduous Forest",
                                              "Mixed Forest",
                                              "Woodland",
                                              "Low Shrub",
                                              "Tall Shrub",
                                              "Open Shrubs",
                                              "Herbaceous",
                                              "Tussock Tundra",
                                              "Sparsely Vegetated",
                                              "Fen",
                                              "Bog",
                                              "Shallows/littoral",
                                              "Barren",
                                              "Water"))

# WorldClim bioclimatic variables
library(geodata)
bioclim <- worldclim_country(var = "bio", country = "CA", res = 0.5, path = "./Data/WorldClim/") %>%
  project(y = covar, method = "bilinear", threads = 20)

covar <- c(covar, bioclim)

# Extract covariate data at phenocam locations
data <- terra::extract(covar, vect(pheno), xy = TRUE) %>%
  select(-ID) %>%
  bind_cols(pheno %>% st_drop_geometry())

# Combine with phenology data
data.merged <- merge(data, pheno_doy, by.x = "name", by.y = "plot") %>%
  filter(year == 2023)

# Test random forest
library(randomForest)

set.seed(123)
model.rf <- randomForest(SnowFree_100 ~ dem + slope + twi + landcover + aspect_ns + aspect_ew + srai +
                           wc2.1_30s_bio_1 + wc2.1_30s_bio_2 + wc2.1_30s_bio_3 + wc2.1_30s_bio_4 +
                           wc2.1_30s_bio_5 + wc2.1_30s_bio_6 + wc2.1_30s_bio_7 + wc2.1_30s_bio_8 +
                           wc2.1_30s_bio_9 + wc2.1_30s_bio_10 + wc2.1_30s_bio_11 + wc2.1_30s_bio_12 +
                           wc2.1_30s_bio_13 + wc2.1_30s_bio_14 + wc2.1_30s_bio_15 + wc2.1_30s_bio_16 +
                           wc2.1_30s_bio_17 + wc2.1_30s_bio_18 + wc2.1_30s_bio_19,
                         data = data.merged,
                         importance = TRUE,
                         na.action = na.omit,
                         ntree = 500)
model.rf
varImpPlot(model.rf)

# Predict across the landscape
phenology_map <- terra::predict(covar, model.rf)
