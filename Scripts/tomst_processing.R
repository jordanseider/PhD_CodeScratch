## TOMST data processing
library(tidyverse)
library(grid)
library(scales)

# Create list of all TOMST loggers' csv data files
data.list <- list.files(path = "C:/Users/jseider.stu/Sync/Data/Phenology/QHI_TOMST_Data_2025",
                         pattern = "*.csv",
                         full.names = TRUE) %>%
  
# keep only files that contain the string "QHI_TOMST_" in the file name (other data don't have the same structure and cause errors)
  .[str_detect(., "_QHI_TOMST_")]

# Combine the data from all loggers into a single data frame with new column to specify the source
data.all <- data.frame(); for (file in data.list) {
  temp.data <- read.csv(file, sep = ";", header = FALSE)

  temp.data$source <- str_extract(file, "QHI_TOMST_\\d{2}")

  names(temp.data) <- c("measurement",
                        "datetime_UTC",
                        "timezone",
                        "temp_below",
                        "temp_surface",
                        "temp_air",
                        "moisture",
                        "logger",
                        "shake",
                        "errorflag",
                        "site_id")
  
  temp.data$datetime_UTC = as.POSIXct(temp.data$datetime_UTC, 
                                      format = "%Y.%m.%d %H:%M", 
                                      tz     = "UTC")
  
  temp.data$datetime_YST = as.POSIXct(temp.data$datetime_UTC, 
                                      format = "%Y.%m.%d %H:%M", 
                                      tz     = "America/Whitehorse")
  
  data.all <- rbind(data.all, temp.data)
}

# Create new data frame with summary of climate variables at each site each month
data.monthly <- data.all %>%
  
  mutate(year  = as.numeric(format(datetime_YST, "%Y")),
         month = as.numeric(format(datetime_YST, "%m"))) %>%
  
  group_by(site_id, year, month) %>%
  
  summarize(mean_temp_air     = mean(temp_air,     na.rm = TRUE),
            mean_temp_surface = mean(temp_surface, na.rm = TRUE),
            mean_temp_below   = mean(temp_below,   na.rm = TRUE),
            mean_moisture     = mean(moisture,     na.rm = TRUE),
            .groups = "drop") %>%
  
  arrange(site_id, year, month) %>%
  
  filter(year > 2022,
         # exclude site_id with value greater than 20 (greater than 20, no associated phenocam)
         site_id %in% paste0("QHI_TOMST_", str_pad(1:20, 2, pad = "0"))) 


## Function to plot Tomst data:
plot.tomst <- function(site, data = data.all, from, to, plot.monthly = TRUE){ #site must be written as "QHI_TOMST_XX"

  if(!is.null(from)){
    data <- data %>%
      filter(datetime_YST >= as.POSIXct(from, tz = "UTC"))
  }
  if(!is.null(to)){
    data <- data %>%
      filter(datetime_YST <= as.POSIXct(to, tz = "UTC"))
  }
  
# TOMST data visualization with legend
plot.1 <- data %>%
  filter(grepl(site, site_id)) %>%

  ggplot() +
  geom_line(aes(x = datetime_YST,
                y = temp_air,
                color = "Air"), alpha = 0.85) +
  geom_line(aes(x = datetime_YST,
                y = temp_surface,
                color = "Surface"), alpha = 0.85) +
  geom_line(aes(x = datetime_YST,
                y = temp_below,
                color = "Below Surface"), alpha = 0.85) +
  scale_x_datetime(date_labels = "%d-%b-%Y") +
  labs(x = "\nTimestamp (Yukon Standard Time)", y = "Temperature (°C)\n", color = "Temperature Profile") +
  theme_bw() 

# Plot monthly average air temperature at each site with different lines for each year of data
if(plot.monthly == TRUE){
plot.2 <- data.monthly %>%
  filter(site_id == site) %>%
  ggplot() +
  geom_line(aes(x = month, y = mean_temp_air, color = as.factor(year)), linewidth = 1) +
  geom_point(aes(x = month, y = mean_temp_air, color = as.factor(year)), size = 2) +
  labs(x = "Month", y = "Mean Monthly Air Temperature (°C)",
       color = "Year") +
  scale_x_continuous(breaks = 1:12) +
  theme_bw()}

if(plot.monthly == FALSE){
  plot.1 +
    ggtitle(site)
} else {
arranged_plots <- gridExtra::grid.arrange(plot.1, plot.2, ncol = 1,
                                          top = textGrob(site, 
                                                         gp = gpar(fontsize = 14,
                                                                   fontface = "bold"),
                                                         x = 0.05, 
                                                         hjust = 0))
}

}

plot.tomst("QHI_TOMST_38",
           from = "2024-08-07",
           to = "2024-08-15",
           plot.monthly = FALSE)

# Convert data.monthly to wide format for easier comparison with phenocam data
data.monthly.wide <- data.monthly %>%
  pivot_wider(names_from = month,
              values_from = c(mean_temp_air,
                              mean_temp_surface,
                              mean_temp_below,
                              mean_moisture),
              names_glue = "{.value}_month{month}") %>%
  arrange(site_id, year) %>%
  mutate(site = str_replace(site_id, "_TOMST_", "_")) %>%
  select(site_id, site, year,
         starts_with("mean_temp_air"),
         starts_with("mean_temp_surface"),
         starts_with("mean_temp_below"),
         starts_with("mean_moisture"))

# write.csv(data.monthly.wide, "./tomst_wide_processed.csv", row.names = FALSE)
