library(tidyverse)
library(weathercan)
library(sf)
library(lutz)

stations_search("Herschel Island")
qhi <- weather_dl(station_ids = 1560, start = "2015-01-01", end = "2025-10-31")

summer <- qhi %>%
  mutate(
    season = case_when(
      month %in% c("01", "02", "03", "04", "11", "12") ~ "winter",
      month %in% c("05", "06") ~ "spring",
      month %in% c("07", "08") ~ "summer",
      month %in% c("09", "10") ~ "autumn"
    )
  ) %>%
  filter(season == "summer" & !is.na(temp)) %>%
  group_by(date) %>%
  slice_max(order_by = temp, n = 1, with_ties = FALSE)

byyear <- summer %>%
  group_by(year) %>%
  summarize(
    n = n(),
    days_above_5 = sum(temp > 5, na.rm = TRUE),
    days_above_10 = sum(temp > 10, na.rm = TRUE),
    days_above_15 = sum(temp > 15, na.rm = TRUE),
    days_above_20 = sum(temp > 20, na.rm = TRUE)
  ) %>%
  ungroup()

dataplot <- byyear %>%
  pivot_longer(
    cols = c(days_above_15, days_above_20),
    names_to = "threshold",
    values_to = "days"
  ) %>%
  mutate(year_label = paste0(year, "\n(n=", n, ")")) %>%
  ggplot(aes(x = year_label, y = days, color = threshold, group = threshold)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_manual(
    values = c("days_above_15" = "#0a697d", "days_above_20" = "#ab2a42"),
    labels = c("Total Days > 15°C", "Total Days > 20°C")
  ) +
  labs(
    x = "\nYear",
    y = "Number of Days\n",
    color = "July 1 to August 31"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )

ggsave(
  "./Figures/SummerHeat.png",
  dataplot,
  height = 4,
  width = 7,
  units = "in"
)
