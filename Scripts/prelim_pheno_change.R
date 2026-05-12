library(terra)
library(tidyverse)

pheno1 <- rast("E:/PhenologyDataFromSeamore/Processed/phenology_50PCGI.tif")
pheno2 <- rast("E:/PhenologyDataFromSeamore/Processed/phenology_OGI.tif")

get_slope <- function(y) {
  # Check if the pixel is NoData (e.g., masked out water or clouds)
  if (all(is.na(y))) {
    return(NA)
  }

  slope <- cov(2016:2023, y, use = "complete.obs") / var(2016:2023)
  return(slope)
}

slope_raster_1 <- app(pheno1, fun = get_slope, cores = 12)
cleaned_slope_1 <- clamp(slope_raster_1, lower = -5, upper = 5, values = TRUE)

slope_raster_2 <- app(pheno2, fun = get_slope, cores = 12)
cleaned_slope_2 <- clamp(slope_raster_2, lower = -5, upper = 5, values = TRUE)

trend_colors <- colorRampPalette(c("red", "white", "forestgreen"))(100)
bolder_colors <- colorRampPalette(c(
  "darkred",
  "red",
  "red", # Duplicating red holds the solid color longer
  "white", # The white transition zone is now squeezed into a tiny fraction of the ramp
  "forestgreen", # Duplicating green holds the solid color longer
  "forestgreen",
  "darkgreen"
))(100)

par(mfrow = c(1, 1))

plot(
  cleaned_slope_2,
  col = bolder_colors,
  main = "15% Green-up Trend (2016 - 2023)",
  plg = list(title = "Days/Yr")
)

plot(
  cleaned_slope_1,
  col = bolder_colors,
  main = "50% Green-up Trend (2016 - 2023)",
  plg = list(title = "Days/Yr")
)


data1_summary <- pheno1 %>%
  spatSample(size = 10000, as.df = TRUE, na.rm = TRUE, cells = TRUE) %>%
  pivot_longer(cols = -cell, names_to = "year", values_to = "doy_50pcgi") %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(doy_50pcgi)),
    median = round(median(doy_50pcgi)),
    sd = sd(doy_50pcgi)
  ) %>%
  ungroup() %>%
  mutate(year = as.integer(year))

ggplot(data1_summary, aes(x = year, y = sd)) +
  geom_line(color = "dodgerblue", linewidth = 1.2) +
  geom_point(color = "dodgerblue", size = 3) +
  ylim(0, 10)

data1_clamped <- pheno1 %>%
  clamp(lower = 120, upper = 225, values = FALSE) %>%
  spatSample(
    size = 10000,
    as.df = TRUE,
    na.rm = TRUE,
    cells = TRUE,
    xy = TRUE
  ) %>%
  pivot_longer(
    cols = -c(cell, x, y),
    names_to = "year",
    values_to = "doy_50pcgi"
  ) %>%
  select(-x)

ggplot(data1_clamped, aes(x = year, y = doy_50pcgi, group = cell)) +
  geom_line(alpha = 0.02)

data2_summary <- pheno2 %>%
  spatSample(size = 10000, as.df = TRUE, na.rm = TRUE, cells = TRUE) %>%
  pivot_longer(cols = -cell, names_to = "year", values_to = "doy_ogi") %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(doy_ogi)),
    median = round(median(doy_ogi)),
    sd = sd(doy_ogi)
  ) %>%
  ungroup() %>%
  mutate(year = as.integer(year))

sd_plot <- ggplot(data2_summary, aes(x = year, y = sd)) +

  geom_line(aes(color = "15%")) +
  geom_point(aes(color = "15%"), size = 2) +

  geom_line(data = data1_summary, aes(x = year, y = sd, colour = "50%")) +
  geom_point(
    data = data1_summary,
    aes(x = year, y = sd, colour = "50%"),
    size = 2
  ) +

  ylim(0, 15) +
  scale_color_manual(
    name = "Onset of Greenness",
    values = c("15%" = "dodgerblue", "50%" = "darkolivegreen")
  ) +
  scale_x_continuous(
    breaks = seq(min(data2_summary$year), max(data2_summary$year), by = 1)
  ) +

  theme_classic() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  ) +
  labs(x = "\nYear", y = "Standard Deviation of Pixel Values\n")

ggsave(
  "C:/Users/jseider.stu/Sync/Figures/Pheno_Homogeneity.png",
  sd_plot,
  height = 4,
  width = 7,
  units = "in"
)

data2_clamped <- pheno2 %>%
  clamp(lower = 120, upper = 200, values = FALSE) %>%
  spatSample(
    size = 10000,
    as.df = TRUE,
    na.rm = TRUE,
    cells = TRUE,
    xy = TRUE
  ) %>%
  pivot_longer(
    cols = -c(cell, x, y),
    names_to = "year",
    values_to = "doy_ogi"
  ) %>%
  select(-x)

ggplot(data2_clamped, aes(x = year, y = doy_ogi, group = cell)) +
  geom_line(alpha = 0.02)
