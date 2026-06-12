library(terra)
library(tidyverse)
library(randomForest)

setwd("C:/Users/jseider.stu")

ivvavik <- vect(
  "./Sync/Data/ParksCanada/IvvavikNationalPark/IvvavikNationalPark.shp"
) %>%
  project("EPSG:32607")

ogi_doy <- rast("./Sync/Data/Phenology/MS-LSP/ogi_ivvavik.tif")
crs(ogi_doy) <- crs(ivvavik)
ogi_doy <- crop(ogi_doy, ivvavik, mask = TRUE)
names(ogi_doy) <- paste0("ogi_doy_", names(ogi_doy))

evi_area <- rast("./Sync/Data/Phenology/MS-LSP/eviarea_ivvavik.tif")
crs(evi_area) <- crs(ivvavik)
evi_area <- crop(evi_area, ivvavik, mask = TRUE)
names(evi_area) <- paste0("evi_area_", names(evi_area))

mean_ogidoy <- mean(ogi_doy)
mean_eviarea <- mean(evi_area)

mean_eviarea_norm <- (mean_eviarea - minmax(mean_eviarea)[1]) /
  (minmax(mean_eviarea)[2] - minmax(mean_eviarea)[1])

topo <- c(
  rast("./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"),
  rast("./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_twi_10m.tif")
)
topo$aspect_10m <- NULL
topo <- resample(topo, mean_ogidoy, method = "bilinear")

topo$geomorph <- rast(
  "./Sync/Data/ArcticDEM/IvvavikNP/geomorphons_0_40.tif"
) %>%
  resample(mean_ogidoy, method = "near")

veg <- rast("./Sync/Data/AnnualPFT_ABoVE_Macander/IvvavikNP_2015_TopCover.tif")

clim <- c(
  rast("./Sync/Data/Climate/Ivvavik_monthly_mean_tmax.tif"),
  rast("./Sync/Data/Climate/Ivvavik_gdd_doy150.tif"),
  rast("./Sync/Data/Climate/meanSWE_doy100_120.tif"),
  rast("./Sync/Data/Climate/meanSWE_doy121_150.tif")
) %>%
  project(ogi_doy, method = "bilinear")

set.seed(123)
sample <- spatSample(
  ivvavik,
  size = 25000
)

df <- data.frame(
  #  eviarea = terra::extract(mean_eviarea_norm, sample)[, -1],
  ogi_doy = terra::extract(mean_ogidoy, sample)[, -1],
  terra::extract(topo, sample)[, -1],
  terra::extract(veg, sample)[, -1],
  terra::extract(clim, sample)[, -1]
) %>%
  na.omit()

set.seed(1)
train_idx <- sample(nrow(df), round(nrow(df) * 0.7))

df.train <- df[train_idx, ] %>%
  select(-geomorph) # %>%
#mutate(geomorph = as.factor(geomorph))

set.seed(2)
model.rf <- randomForest(
  ogi_doy ~ .,
  data = df.train,
  importance = TRUE
)
model.rf
varImpPlot(model.rf)

as.data.frame(importance(model.rf))

df.valid <- df[-train_idx, ] %>%
  select(-geomorph) #%>%
#mutate(geomorph = as.factor(geomorph))

predictions <- predict(model.rf, newdata = df.valid)
results_df <- data.frame(
  Actual = df.valid$ogi_doy,
  Predicted = predictions
)
(valid_rmse <- sqrt(mean((results_df$Actual - results_df$Predicted)^2)))
(valid_r2 <- cor(results_df$Actual, results_df$Predicted)^2)

model.lm <- lm(Predicted ~ Actual, data = results_df)

ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3) +
  # Add a 1:1 perfect prediction line
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = paste0(
      "Random Forest Validation Plot\nOLS Slope: ",
      round(coef(model.lm)[2], 2)
    ),
    x = "\nActual EVI Area",
    y = "Predicted EVI Area\n"
  ) +
  # xlim(0, NA) +
  # ylim(0, NA) +
  theme_classic()

partialPlot(
  x = model.rf,
  pred.data = df.train,
  x.var = "GDD_0C_DOY150",
  plot = TRUE,
  rug = TRUE,
  main = "Partial Dependence",
  xlab = "Growing Degree Days (0C) to DOY 150",
  ylab = "Predicted Onset of Green up (15%) DOY"
)
