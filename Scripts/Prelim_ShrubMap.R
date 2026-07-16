library(terra)
library(tidyverse)
library(tidyterra)

setwd("E:/Orndahl_PFT_Biomass/resource_map_doi_10_18739_A2Q52FF2B/data")

# Load raw data
ivvavik_raw <- vect("C:/Users/jseider.stu/Sync/Data/ParksCanada/IvvavikNationalPark/IvvavikNationalPark.shp")
cover_raw   <- rast("C:/Users/jseider.stu/Sync/Data/AnnualPFT_ABoVE_Macander/2010/ABoVE_PFT_Top_Cover_DeciduousShrub_2010.tif") 
biomass_raw <- rast("./pft_agb_deciduousshrub_p50_2010.tif")

# Convert data into Yukon Albers (EPSG 3579)
ivvavik_data <- project(ivvavik_raw, crs(cover_raw))
ivvavik_3579 <- project(ivvavik_raw, "EPSG:3579")

cover   <- crop(cover_raw, ivvavik_data) %>%
  project("EPSG:3579", method = "bilinear") %>%
  mask(ivvavik_3579)

biomass <- crop(biomass_raw, ivvavik_data) %>%
  project(cover, method = "bilinear") %>%
  mask(ivvavik_3579)

# Reclassify continuous percent values to rank groups 
cover_cat <- classify(
  cover,
  rcl = matrix(
    c(0,  0,   0,
      0,  10,  1,
      10, 30,  2,
      30, 70,  3,
      70, 100, 4),
    ncol = 3,
    byrow = TRUE
  ),
  include.lowest = TRUE
)

biomass_cat <- classify(
  biomass,
  rcl = matrix(
    c(
      0,   0,   0,
      0,   100, 1,
      100, 250, 2,
      250, 500, 3,
      500, global(biomass, "max", na.rm = TRUE)[[1]] + 1, 4
    ),
    ncol = 3,
    byrow = TRUE
  ),
  include.lowest = TRUE
)

# Combine rank values for each dataset 
combined <- ifel(
  cover_cat == 0,
  0, # If TRUE: force value to 0
  (cover_cat * 10) + biomass_cat
) %>% # If FALSE: calculate the combined ID (first digit is cover, second is biomass)
  as.factor()

# Reclassify the combined rank values into single ranks
combined_simple <- classify(
  combined,
  rcl = matrix(
    c(
      0,  0,
      10, 1,
      11, 1,
      12, 1,
      13, 2,
      20, 1,
      21, 1,
      22, 1,
      23, 2,
      24, 2,
      31, 3,
      32, 3,
      33, 4,
      34, 4,
      44, 5
    ),
    ncol = 2,
    byrow = T
  )
) %>%
  as.factor()

levels(combined_simple) <- data.frame(
  ID = 0:5,
  cover = c(
    "no cover - no biomass",
    "low cover - low biomass",
    "low cover - high biomass",
    "high cover - low biomass",
    "high cover - high biomass",
    "highest cover - highest biomass"
  )
)

# Calculate the combined area of each rank
area_totals <- expanse(combined_simple, unit = "km", byValue = TRUE) %>%
  rename(area_km2 = area) %>%
  mutate(
    perc = (area_km2 / sum(area_km2)) * 100,
    perc = format(round(perc, 1), scientific = FALSE)
  )

custom_labels <- setNames(
  paste0(area_totals$value, " (", area_totals$perc, "%)"),
  area_totals$value
)

# Plot data
(shrub_plot <- ggplot() +
  geom_spatraster(data = combined_simple,
                  maxcell = 2e6) +
  scale_fill_manual(
    name = "Deciduous Shrubs - 2010",
    values = c(
      "no cover - no biomass" = "#fffcf2",
      "low cover - low biomass" = "#c9cba3",
      "low cover - high biomass" = "#ffe1a8",
      "high cover - low biomass" = "#e26d5c",
      "high cover - high biomass" = "#723d46",
      "highest cover - highest biomass" = "#472d30"
    ),
    labels = custom_labels,
    na.translate = FALSE
  ) +
    xlim(c(148977, 275000)) +
    ylim(c(1583114, 1713271)) +
  theme_bw() +
  theme(legend.text = element_text(size = 10)))

# Save plot
ggsave(
  "C:/Users/jseider.stu/Sync/Figures/shrub_plot2.png",
  shrub_plot,
  width = 8,
  height = 6.5,
  units = "in"
)
