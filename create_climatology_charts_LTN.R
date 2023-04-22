# Create climatology charts - LTN COLUMNS INCLUDED IN DATA FILE

# This script reads a weather data file (.csv) with long-term normal (LTN)
# columns already contained within the file.
# The outputs are climatology charts visualizing current and LTN
# weekly climate variables.

# This code is based on / incorporates the Create aWhere Chart function,
# code here on GitHub:
#  https://github.com/aWhereAPI/aWhere-R-Charts/blob/dev/R/generateaWhereChart.R

# install / load required R packages --------------------------------------

library(tidyverse) # includes packages such as readr, dplyr, tidyr
library(zoo)
library(ggplot2)



# User-defined inputs -----------------------------------------------------

# Specify the file (.csv) containing weather data.
data_filename <- "data/Kenya_TA00320_01-01_to_04-29_FAKE-PET.csv"

# Specify the place name, latitude, and longitude coordinates
lat <- "1.06"
lon <- "35.03"
place_name <- "Kibomet, Kenya"

# Specify dates of interest using YYYY-MM-DD format
# NOTE: These dates should not exceed 365 days total
date_start <- "2023-01-01"
date_end <- "2023-04-29"

# --------------------------------------------------------------------------

# create output directory if it does not already exist
if(!exists("outputs")){
  dir.create("outputs")}

# read data file (.csv) into a tibble.
df <- readr::read_csv(data_filename)

# The data already contains LTN values, so this script does not need to calculate them.

# This line combines the dates of interest into a single vector
days <- c(date_start, date_end)


# Create chart title
chart_title <- paste0("Weekly climate chart for ", place_name, " ("
                      , lat, ", ", lon, ") "
                      , "\n", days[1], " to ", days[2])

# Colors for chart geometries
color_pre_curr <-"#94A5BB"
color_pre_LTN <- "#0F3564"
color_maxT_curr <- "#f46d43"
color_maxT_LTN <- "#a50026"

dataToUse <- data.table::as.data.table(data.table::copy(df))

# Subset the period of time the user wants charted to make an accumulation of "CURRENT time"
# Subset the data if the user specifies
if (is.null(date_start) == FALSE) {
  dataToUse <- dataToUse[date >= as.Date(date_start)]
}
if (is.null(date_end) == FALSE) {
  dataToUse <- dataToUse[date <= as.Date(date_end)]
}
if (nrow(dataToUse) == 0) {
  stop('Current settings result in no data being plotted\n')
}
# Add a check for the length of the current timeframe of interest.
if (as.numeric(difftime(as.Date(date_end), as.Date(date_start), units = "days")) > 365){
  stop('Date range of interest cannot exceed 365 days.\n')
}

# temporally aggregate data over period of 7 days (weekly)
daysToAggregateOver <- 7

# calculate CURRENT weekly accumulated precipitation
dataToUse$current_weekly_precip <- zoo::rollapply(data = dataToUse$rainAccumulationSum,
               width = daysToAggregateOver,
               FUN = sum,
               align = "right",
               na.rm = TRUE,
               fill = NA,
               partial = TRUE)

# calculate CURRENT weekly maximum temperature
dataToUse$current_weekly_maxT <- zoo::rollapply(data = dataToUse$temperatureMax,
                                                  width = daysToAggregateOver,
                                                  FUN = max,
                                                  align = "right",
                                                  na.rm = TRUE,
                                                  fill = NA,
                                                  partial = TRUE)

# calculate CURRENT weekly minimum temperature
dataToUse$current_weekly_minT <- zoo::rollapply(data = dataToUse$temperatureMin,
                                                width = daysToAggregateOver,
                                                FUN = min,
                                                align = "right",
                                                na.rm = TRUE,
                                                fill = NA,
                                                partial = TRUE)

# calculate CURRENT weekly PET to be divided by weekly precip for P/PET
dataToUse$current_weekly_PET <- zoo::rollapply(data = dataToUse$evapotranspirationSum,
                                                width = daysToAggregateOver,
                                                FUN = sum,
                                                align = "right",
                                                na.rm = TRUE,
                                                fill = NA,
                                                partial = TRUE)

# calculate CURRENT weekly P/PET. Divide weekly precip sums by weekly PET sums.
dataToUse$current_weekly_PPET <- dataToUse$current_weekly_precip / dataToUse$current_weekly_PET


# Aggregate long-term normal precip from existing column labeled LTN ---------------------
# temporally aggregate data over period of 7 days (weekly)

# calculate LTN weekly accumulated precipitation
dataToUse$LTN_weekly_precip <- zoo::rollapply(data = dataToUse$LTNpre,
                                                  width = daysToAggregateOver,
                                                  FUN = sum,
                                                  align = "right",
                                                  na.rm = TRUE,
                                                  fill = NA,
                                                  partial = TRUE)

# calculate LTN weekly max temperature
dataToUse$LTN_weekly_maxT <- zoo::rollapply(data = dataToUse$LTNMaxT,
                                                width = daysToAggregateOver,
                                                FUN = max,
                                                align = "right",
                                                na.rm = TRUE,
                                                fill = NA,
                                                partial = TRUE)

# calculate LTN weekly min temperature
dataToUse$LTN_weekly_minT <- zoo::rollapply(data = dataToUse$LTNMinT,
                                            width = daysToAggregateOver,
                                            FUN = min,
                                            align = "right",
                                            na.rm = TRUE,
                                            fill = NA,
                                            partial = TRUE)

# calculate LTN weekly PET
dataToUse$LTN_weekly_PET <- zoo::rollapply(data = dataToUse$LTNPET,
                                            width = daysToAggregateOver,
                                            FUN = sum,
                                            align = "right",
                                            na.rm = TRUE,
                                            fill = NA,
                                            partial = TRUE)

# calculate LTN weekly P/PET
dataToUse$LTN_weekly_PPET <- dataToUse$LTN_weekly_precip / dataToUse$LTN_weekly_PET


# take every Nth row (7th for weekly stats)
dataToUse_weeklyStats <- dataToUse[seq(from = 1
                                   ,to = nrow(dataToUse)
                                   ,by = daysToAggregateOver),]


# CREATE CLIMATOLOGY CHART ----------------------------------------------

# Current and LTN weekly precipitation along with maximum temperature

# bar plot with weekly accumulated precip during CURRENT timeframe of interest
chart <- dataToUse_weeklyStats %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(aes(x = date,
                        y = current_weekly_precip,
                        fill = color_pre_curr)) +
  theme_bw() +
  labs(title = chart_title,
       x = "Date",
       y = "Weekly accumulated precipitation (mm)")+
  scale_fill_identity(name = NULL,
                      breaks = c(color_pre_curr),
                      labels = c("Current precip"),
                      guide = "legend") +
  theme(legend.position="bottom")

# display chart with current weekly precipitation
chart

# calculate scale shift for second y-axis (max temperatures)
# code from: https://finchstudio.io/blog/ggplot-dual-y-axes/
max_first  <- max(dataToUse_weeklyStats$current_weekly_precip)   # Specify max of first y axis
max_second <- max(dataToUse_weeklyStats$current_weekly_maxT,
                  dataToUse_weeklyStats$LTN_weekly_maxT,
                  na.rm = TRUE) # Specify max of second y axis
min_first  <- min(dataToUse_weeklyStats$current_weekly_precip)   # Specify min of first y axis
min_second <- min(dataToUse_weeklyStats$current_weekly_maxT,
                  dataToUse_weeklyStats$LTN_weekly_maxT,
                  na.rm = TRUE) # Specify min of second y axis
# scale and shift variables calculated based on desired mins and maxes
scale = (max_second - min_second)/(max_first - min_first)
shift = min_first - min_second
# Function to scale secondary axis
scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}
# Function to scale secondary variable values
inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}

# Add the other geometries to the chart
chart_with_LTN <- chart +

  # LTN precip
  ggplot2::geom_line(data = dataToUse_weeklyStats,
                     aes(x = date, y = LTN_weekly_precip,
                         color = color_pre_LTN))+

  # CURRENT Max Temperature
  ggplot2::geom_line(data = dataToUse_weeklyStats,
                     aes(x = date,
                         y = inv_scale_function(current_weekly_maxT,scale,shift),
                         color = color_maxT_curr)) +

  # LTN Max temperature
  ggplot2::geom_line(data = dataToUse_weeklyStats,
                     aes(x = date,
                         y = inv_scale_function(LTN_weekly_maxT,scale,shift),
                         color = color_maxT_LTN)) +


  # create a second y-axis on the right side of the plot
  scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
                                         name="Celsius")) +
  # specify the geometry colors and labels in the legend
  scale_color_identity(name = NULL,
                       breaks = c(color_pre_LTN, color_maxT_LTN, color_maxT_curr),
                       labels = c("LTN precip", "LTN max T", "Current max T"),
                       guide = "legend") +
  theme(legend.position="bottom")
# Display the chart
chart_with_LTN



# write chart to image file
if(!exists("outputs")){
  dir.create("outputs")}

ggplot2::ggsave(filename = paste0("outputs/", place_name, "_",
                                  "weekly-climate-chart.png"),
                width = 8, height = 6, units = "in",
                plot = chart_with_LTN)













# CREATE INDIVIDUAL CHART PER WEATHER VARIABLE  -------------------------------
# Plot the current (bars) and LTN (solid line) with colors:
color_var_curr <- "#94A5BB"
color_var_LTN <- "#0F3564"

# PRECIPIRATION (RAINFALL)
title_rainfall <- paste0("Weekly rainfall: current and Long-Term Normal (LTN) \nfor ", place_name, " ("
                      , lat, ", ", lon, ") "
                      , days[1], " to ", days[2])

chart_rainfall <- ggplot2::ggplot(dataToUse_weeklyStats, aes(date)) +
  ggplot2::geom_col(aes(y = current_weekly_precip, fill = color_var_curr)) +
  ggplot2::geom_line(aes(y = LTN_weekly_precip, color = color_var_LTN)) +
  scale_x_date(date_labels = "%B %d"
               #,date_breaks = "2 weeks"
               #,limits = c(as.Date(date_start), as.Date(date_end))
               ) +
  labs(title = title_rainfall,
       x = "Week start date",
       y = "Weekly accumulated rainfall (mm)")+
  # format the legend items based on their color and names
  scale_fill_identity(name = NULL,
                      breaks = c(color_var_curr),
                      labels = c("Current rainfall"),
                      guide = "legend") +
  scale_color_identity(name = NULL,
                       breaks = c(color_var_LTN),
                       labels = c("LTN Rainfall"),
                       guide = "legend") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="right")

# display & save chart
chart_rainfall

ggplot2::ggsave(filename = paste0("outputs/", place_name, "_",
                                  "weekly-rainfall.png"),
                width = 8, height = 6, units = "in",
                plot = chart_rainfall)





# MAXIMUM TEMPERATURE ------------------------------------------------
title_maxT <- paste0("Weekly maximum temperature: current and Long-Term Normal (LTN) \nfor ", place_name, " ("
                         , lat, ", ", lon, ") "
                         , days[1], " to ", days[2])

chart_maxT <- ggplot2::ggplot(dataToUse_weeklyStats, aes(date)) +
  ggplot2::geom_col(aes(y = current_weekly_maxT, fill = color_var_curr)) +
  ggplot2::geom_line(aes(y = LTN_weekly_maxT, color = color_var_LTN)) +
  scale_x_date(date_labels = "%B %d"
  ) +
  labs(title = title_maxT,
       x = "Week start date",
       y = "Weekly maximum temperature (deg C)")+
  # format the legend items based on their color and names
  scale_fill_identity(name = NULL,
                      breaks = c(color_var_curr),
                      labels = c("Current max temp"),
                      guide = "legend") +
  scale_color_identity(name = NULL,
                       breaks = c(color_var_LTN),
                       labels = c("LTN max temp "),
                       guide = "legend") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="right")

# display & save chart
chart_maxT

ggplot2::ggsave(filename = paste0("outputs/", place_name, "_",
                                  "weekly-maxT.png"),
                width = 8, height = 6, units = "in",
                plot = chart_maxT)


# MINIMUM TEMPERATURE ------------------------------------------------
title_minT <- paste0("Weekly minimum temperature: current and Long-Term Normal (LTN) \nfor ", place_name, " ("
                     , lat, ", ", lon, ") "
                     , days[1], " to ", days[2])

chart_minT <- ggplot2::ggplot(dataToUse_weeklyStats, aes(date)) +
  ggplot2::geom_col(aes(y = current_weekly_minT, fill = color_var_curr)) +
  ggplot2::geom_line(aes(y = LTN_weekly_minT, color = color_var_LTN)) +
  scale_x_date(date_labels = "%B %d"
  ) +
  labs(title = title_minT,
       x = "Week start date",
       y = "Weekly minimum temperature (deg C)")+
  # format the legend items based on their color and names
  scale_fill_identity(name = NULL,
                      breaks = c(color_var_curr),
                      labels = c("Current min temp"),
                      guide = "legend") +
  scale_color_identity(name = NULL,
                       breaks = c(color_var_LTN),
                       labels = c("LTN min temp"),
                       guide = "legend") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="right")

# display & save chart
chart_minT

ggplot2::ggsave(filename = paste0("outputs/", place_name, "_",
                                  "weekly-minT.png"),
                width = 8, height = 6, units = "in",
                plot = chart_minT)



# P / PET ------------------------------------------------
title_PPET <- paste0("Weekly P/PET: current and Long-Term Normal (LTN) \nfor ", place_name, " ("
                     , lat, ", ", lon, ") "
                     , days[1], " to ", days[2])

chart_PPET <- ggplot2::ggplot(dataToUse_weeklyStats, aes(date)) +
  ggplot2::geom_col(aes(y = current_weekly_PPET, fill = color_var_curr)) +
  ggplot2::geom_line(aes(y = LTN_weekly_PPET, color = color_var_LTN)) +
  scale_x_date(date_labels = "%B %d"
  ) +
  labs(title = title_PPET,
       x = "Week start date",
       y = "Weekly P/PET")+
  # format the legend items based on their color and names
  scale_fill_identity(name = NULL,
                      breaks = c(color_var_curr),
                      labels = c("Current P/PET"),
                      guide = "legend") +
  scale_color_identity(name = NULL,
                       breaks = c(color_var_LTN),
                       labels = c("LTN P/PET"),
                       guide = "legend") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="right")

# display & save chart
chart_PPET

ggplot2::ggsave(filename = paste0("outputs/", place_name, "_",
                                  "weekly-PPET.png"),
                width = 8, height = 6, units = "in",
                plot = chart_PPET)


