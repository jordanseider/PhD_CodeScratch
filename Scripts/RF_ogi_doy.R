# =============================================================================
# Random Forests Analysis of Green Up - Ivvavik National Park
# Jordan Seider
# =============================================================================

# -----------------------------------------------------------------------------
# Environment Setup & Configuration
# -----------------------------------------------------------------------------

library(terra)
library(tidyverse)
library(ranger)
library(spatialRF)
library(pdp)
library(vip)

setwd("C:/Users/jseider.stu")

# -------------------------------------------------------------------------
# Load Aligned Data (From Previous Pipeline)
# -------------------------------------------------------------------------

# Response Variable
# Taking the mean across all years (2016-2023) to create a single spatial layer
ogi <- rast("./Sync/Data/Phenology/MS-LSP/ogi_ivvavik.tif") |>
  mean(na.rm = TRUE) |>
  setNames("ogi_doy")

# Predictors
topo <- c(
  rast("./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"),
  rast("./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_twi_10m.tif"),
  rast("./Sync/Data/ArcticDEM/IvvavikNP/geomorphons_0_40.tif"),
  rast("./Sync/Data/ArcticDEM/IvvavikNP/distance_to_coast_m.tif") |>
    project(ogi) |>
    mask(ivvavik)
)
topo$aspect_10m <- NULL

veg <- rast("./Sync/Data/AnnualPFT_ABoVE_Macander/IvvavikNP_2015_TopCover.tif")

clim <- c(
  # Must acknowledge the DayMet data resolution at 1km is far coarser than 10m topo data
  rast("./Sync/Data/Climate/Ivvavik_dayl_hrs_150.tif"),
  rast("./Sync/Data/Climate/Ivvavik_monthly_mean_tmax.tif"),
  rast("./Sync/Data/Climate/Ivvavik_gdd_doy150.tif") #,
  #rast("./Sync/Data/Climate/meanSWE_doy100_120.tif"),
  #rast("./Sync/Data/Climate/meanSWE_doy121_150.tif")
)
names(clim)[names(clim) == "lyr.1"] <- "gdd_doy_150"

# Create a single master stack of all variables
model_stack <- c(ogi, topo, veg, clim)

model_stack <- subset(
  model_stack,
  c(
    "March_Mean_Tmax",
    "April_Mean_Tmax",
    "aspect_ns_10m",
    "broadleaf_tree_2015",
    "cumulative_dayl_to_doy150"
  ),
  negate = TRUE
)

variable_names <- data.frame(
  layer = c(
    "ogi_doy",
    "dem_10m",
    "slope_10m",
    "TPI_10m",
    "aspect_ns_10m",
    "aspect_ew_10m",
    "twi_10m",
    "form",
    "broadleaf_tree_2015",
    "conifer_tree_2015",
    "deciduous_shrub_2015",
    "evergreen_shrub_2015",
    "forb_2015",
    "graminoid_2015",
    "lichen_2015",
    "cumulative_dayl_to_doy150",
    "March_Mean_Tmax",
    "April_Mean_Tmax",
    "May_Mean_Tmax",
    "June_Mean_Tmax",
    "gdd_doy_150",
    "mean_swe_100_120",
    "mean_swe_121_150",
    "distance_to_coast_m"
  ),
  name = c(
    "Onset of Greenness",
    "Elevation (m)",
    "Slope (°)",
    "Topographic Position",
    "Northness",
    "Eastness",
    "Topographic Wetness",
    "Geomorph",
    "Broadlead Tree Cover (%)",
    "Conifer Tree Cover (%)",
    "Deciduous Shrub Cover (%)",
    "Evergreen Shrub Cover (%)",
    "Forb Cover (%)",
    "Graminoid Cover (%)",
    "Lichen Cover (%)",
    "Cumulative Daylight Hours to DOY 150",
    "Average Max Temp in March (°C)",
    "Average Max Temp in April (°C)",
    "Average Max Temp in May (°C)",
    "Average Max Temp in June (°C)",
    "Growing Degree Days (>0°C up to DOY 150)",
    "Mean SWE from DOY 100 to 120",
    "Mean SWE from DOY 121 to 150",
    "Distance from Coast (m)"
  )
) |>
  deframe()

# -------------------------------------------------------------------------
# Extract Training/Validation Data
# -------------------------------------------------------------------------

# Extract random pixels directly from the stack.
set.seed(123)
df <- spatSample(
  model_stack,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  xy = TRUE
) |>
  mutate(form = as.factor(form)) # Geomorphology variable


# -------------------------------------------------------------------------
# Train Non-spatial Random Forest
# -------------------------------------------------------------------------

rf.ranger <- ranger(
  ogi_doy ~ .,
  data = df[, !(names(df) %in% c("x", "y"))],
  num.trees = 500,
  importance = "permutation",
  scale.permutation.importance = FALSE
)
rf.ranger


# -------------------------------------------------------------------------
# Variable Importance Plot
# -------------------------------------------------------------------------

(vip.plot <- vip(
  rf.ranger,
  all_permutations = TRUE,
  geom = "point",
  aesthetics = list(size = 4)
) +
  geom_segment(
    aes(x = Variable, xend = Variable, y = 0, yend = Importance),
    linewidth = 0.75
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_x_discrete(labels = function(x) {
    ifelse(is.na(variable_names[x]), x, variable_names[x])
  }) +
  ylab("\nImportance") +
  theme_classic(base_size = 16))

ggsave(
  filename = "Ivvavik_GreenUp_Importance.png",
  plot = vip.plot,
  width = 10,
  height = 6,
  dpi = 300,
  units = "in"
)


# -------------------------------------------------------------------------
# Train Spatial Random Forest
# -------------------------------------------------------------------------

# Isolate spatial coordinates
xy <- df[, c("x", "y")]

# Dropping the response, coordinates, and the geomorph variable (spatialRF doesn't handle categorical very well)
pred_vars <- setdiff(names(df), c("ogi_doy", "x", "y", "form"))

# Fit the Spatial Random Forest using Moran's Eigenvector Maps
spatial.rf <- rf_spatial(
  data = df,
  dependent.variable.name = "ogi_doy",
  predictor.variable.names = pred_vars,
  distance.matrix = as.matrix(dist(xy)),

  # Spatial scales (m) at which Moran's Eigenvector Map (MEM) eigenvectors are computed
  # Covers fine to roughly half the max pairwise distance (132 km)
  distance.thresholds = c(0, 1000, 15000, 30000, 65000),

  # Sequentially adds MEMs that most reduce residual spatial autocorrelation
  method = "mem.moran.sequential",

  # Ranger arguments to run random forest model
  ranger.arguments = list(num.trees = 500, importance = "permutation"),
  n.cores = 20,
  seed = 2
)
print(spatial.rf)

# Residual spatial autocorrelation -- want Moran's I near zero at all lags
spatialRF::plot_moran(spatial.rf)
# How many MEMs were selected and at which thresholds
spatial.rf$spatial.predictors.selected
spatialRF::plot_collinearity(spatial.rf)

plot_importance(spatial.rf)
