# Create climatology charts - LTN PRE-CALCULATED & CONTAINED WITHIN FILE.

# This script reads a weather data file (.csv) with long-term normal (LTN)
# values pre-calculated and contained within the file.
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
data_filename <- "data/Kenya_TA00320_01-01_to_04-29.csv"

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

# temporally aggregate data over period of 7 days (weekly)
daysToAggregateOver <- 7

# calculate CURRENT weekly accumulated precipitation
dataToUse$current_weekly_precip <- zoo::rollapply(data = dataToUse$precipitationAccumulationSum,
               width = daysToAggregateOver,
               FUN = sum,
               align = "right",
               na.rm = TRUE,
               fill = NA,
               partial = TRUE)

# calculate CURRENT weekly max temperature
dataToUse$current_weekly_maxT <- zoo::rollapply(data = dataToUse$temperatureMax,
                                                  width = daysToAggregateOver,
                                                  FUN = max,
                                                  align = "right",
                                                  na.rm = TRUE,
                                                  fill = NA,
                                                  partial = TRUE)

# take every Nth row (7th for weekly stats)
dataToUse_current <- dataToUse[seq(from = daysToAggregateOver + 1
                           ,to = nrow(dataToUse)
                           ,by = daysToAggregateOver),]


# Calculate long term normal precip and add column labeled LTN ---------------------
# JC says the LTN data will be available from the API in the future.
# Until then, I will manually calculate the LTN data.

monthDay_start <- format(as.Date(date_start), "%m-%d")
monthDay_end <- format(as.Date(date_end), "%m-%d")

# determine if the current dates of interest span multiple years
# e.g. September 2021 to May 2022 spans 2021 and 2022
dates_span_multi_years <- !(format(as.Date(date_start), "%Y") == format(as.Date(date_end), "%Y"))

# Iterate through the LTN years
for(y in years[1]:years[2]){ # loop through past years, to calculate LTN
  print(y) # past year in the loop

  # concatenate past year in the loop with the starting/ending month/day
  # to get a date range of interest for subsetting the data
  tmp_date_start <- as.Date(paste(y,monthDay_start,sep="-"),"%Y-%m-%d")
  # check to see if the ending date extends into subsequent year.
  # if so, increment y+1 for proper ending date
  if(dates_span_multi_years){
    tmp_date_end <- as.Date(paste(y+1,monthDay_end,sep="-"),"%Y-%m-%d")
  } else{
    tmp_date_end <- as.Date(paste(y,monthDay_end,sep="-"),"%Y-%m-%d")
  }

  # print the dates of interest for the past year of LTN calculations
  print(paste0("LTN start date: ", tmp_date_start))
  print(paste0("LTN end date: ", tmp_date_end))

  # subset dataframe for LTN date range of interest for past year
  dataToUse <- data.table::as.data.table(data.table::copy(df))
  dataToUse <- dataToUse[date >= as.Date(tmp_date_start)]
  dataToUse <- dataToUse[date <= as.Date(tmp_date_end)]

  # calculate weekly accumulated precip for past year
  dataToUse$past_weekly_precip <- zoo::rollapply(data = dataToUse$precipitationAccumulationSum,
                                                    width = daysToAggregateOver,
                                                    FUN = sum,
                                                    align = "right",
                                                    na.rm = TRUE,
                                                    fill = NA,
                                                    partial = TRUE)
  # calculated weekly max temp for  past year
  dataToUse$past_weekly_maxT <- zoo::rollapply(data = dataToUse$temperatureMax,
                                                 width = daysToAggregateOver,
                                                 FUN = max,
                                                 align = "right",
                                                 na.rm = TRUE,
                                                 fill = NA,
                                                 partial = TRUE)

  # take every Nth row (7th for weekly stats)
  dataToUse_past <- dataToUse[seq(from = daysToAggregateOver + 1
                                     ,to = nrow(dataToUse)
                                     ,by = daysToAggregateOver),]

  # create new column for month-day per row
  dataToUse_past$month_day <- format(as.Date(dataToUse_past$date),
                                                format = "%m-%d")
  dataToUse_past$year_start <- format(as.Date(dataToUse_past$date),
                                     format = "%Y")
  dataToUse_past$week_count <- c(1:nrow(dataToUse_past))

  # during first iteration,
  # create a dataframe to store the past years of weekly precip data
  if(y == years[1]){
    dataToUse_LTN <- data.frame(date = dataToUse_past$date,
                                month_day = dataToUse_past$month_day,
                                year_start = dataToUse_past$year_start,
                                week_count = dataToUse_past$week_count,
                                weekly_precip = dataToUse_past$past_weekly_precip,
                                weekly_maxT = dataToUse_past$past_weekly_maxT)
  } else{
    dataToUse_LTN <- rbind(dataToUse_LTN,
                           data.frame(date = dataToUse_past$date,
                                      month_day = dataToUse_past$month_day,
                                      year_start = dataToUse_past$year_start,
                                      week_count = dataToUse_past$week_count,
                                      weekly_precip = dataToUse_past$past_weekly_precip,
                                      weekly_maxT = dataToUse_past$past_weekly_maxT))
  }
}

# LTN PRECIP
# reshape LTN weekly precip data long to wide, with columns renamed using year
dataToUse_LTN_precip <- dataToUse_LTN %>%
  tidyr::pivot_wider(id_cols = week_count,
                     names_from = year_start,
                     values_from = weekly_precip)

# calculate LTN weekly precip by taking the average of
# weekly aggregated precip across all LTN years
dataToUse_LTN_precip$weekly_precip_LTN <- rowMeans(dataToUse_LTN_precip[,c(as.character(years[1]:years[2]))],
                                            na.rm=TRUE)

# merge date column to be able to plot the LTN values as a function of date
# to match with the current weekly precip data.
# NOTE the year will be irrelevant
# so consider changing this to be month-day instead of YYYY-MM-DD
# VS-TO-DO deal with differing number of weeks between current and LTN df's
dataToUse_LTN_precip$date <- dataToUse_current$date[1:nrow(dataToUse_LTN_precip)]


# LTN MAX TEMP calculations ------------------------------------------------
# reshape LTN weekly max temp data long to wide, with columns renamed using year
dataToUse_LTN_maxT <- dataToUse_LTN %>%
  tidyr::pivot_wider(id_cols = week_count,
                     names_from = year_start,
                     values_from = weekly_maxT)

# calculate LTN weekly max temp by taking the average of
# weekly aggregated max temp across all LTN years
dataToUse_LTN_maxT$weekly_maxT_LTN <- rowMeans(dataToUse_LTN_maxT[,c(as.character(years[1]:years[2]))],
                                                   na.rm=TRUE)

# merge date column to be able to plot the LTN values as a function of date
dataToUse_LTN_maxT$date <- dataToUse_current$date[1:nrow(dataToUse_LTN_maxT)]


# CREATE THE CHART ---------------------------------------------------------

# CURRENT PRECIP bar plot
# bar plot with weekly accumulated precip during CURRENT timeframe of interest
chart <- dataToUse_current %>%
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
max_first  <- max(dataToUse_current$current_weekly_precip)   # Specify max of first y axis
max_second <- max(dataToUse_current$current_weekly_maxT,
                  dataToUse_LTN_maxT$weekly_maxT_LTN,
                  na.rm = TRUE) # Specify max of second y axis
min_first  <- min(dataToUse_current$current_weekly_precip)   # Specify min of first y axis
min_second <- min(dataToUse$current_weekly_maxT,
                  dataToUse_LTN_maxT$weekly_maxT_LTN,
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
  ggplot2::geom_line(data = dataToUse_LTN_precip,
                     aes(x = date,
                         y = weekly_precip_LTN,
                         color = color_pre_LTN))+

  # CURRENT Max Temperature
  ggplot2::geom_line(data = dataToUse_current,
                     aes(x = date,
                         y = inv_scale_function(current_weekly_maxT,scale,shift),
                         color = color_maxT_curr)) +

  # LTN Max temperature
  ggplot2::geom_line(data = dataToUse_LTN_maxT,
                     aes(x = date,
                         y = inv_scale_function(weekly_maxT_LTN,scale,shift),
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

chart_with_LTN


# write chart to image file
if(!exists("outputs")){
  dir.create("outputs")}

ggplot2::ggsave(filename = paste0("outputs/", place_name, "_", "weekly-climate-chart.png"),
                plot = chart_with_LTN)
