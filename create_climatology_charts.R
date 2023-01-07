# Create climatology charts

# This script reads a weather data file (.csv) and outputs a
# climatology chart visualizing current and long-term normal
# weekly climate variables:

# Left y-axis: rainfall (mm)
#   Current precip illustrated by bars
#   Long-term normal precip illustrated by line
# x-axis: date (Month labels) during timeframe of interest


# This code is based on / incorporates the Create aWhere Chart function,
# code here on GitHub:
#  https://github.com/aWhereAPI/aWhere-R-Charts/blob/dev/R/generateaWhereChart.R

# install / load required R packages --------------------------------------

library(tidyverse) # includes packages such as readr, dplyr, tidyr
library(zoo)
library(ggplot2)



# User-defined inputs -----------------------------------------------------

# Specify the file (.csv) containing weather data.
data_filename <- "data/Kenya Kibomet_1.06_35.03_daily-1.csv"

# Specify the place name, latitude, and longitude coordinates
lat <- "1.06"
lon <- "35.03"
place_name <- "Kibomet, Kenya"

# Specify dates of interest using YYYY-MM-DD format
date_start <- "2021-09-01"
date_end <- "2022-05-31"


# --------------------------------------------------------------------------

# create output directory if it does not already exist
if(!exists("outputs")){
  dir.create("outputs")}

# read data file (.csv) into a tibble.
df <- readr::read_csv(data_filename)


# Set the time span over which to calculate the long term normal (LTN):
# the start year of the daily data file up to and through the current year -2

# interpreting "current year" as based on TODAY's date in real life
#years <- c(format(min(df$date), "%Y"), as.numeric(format(Sys.Date(), "%Y")) - 2)

# Interpreting "current year" based on the ending year of the "current" date range of interest
years <- c(format(min(df$date), "%Y"), as.numeric(format(as.Date(date_end), "%Y")) - 2)

# Print a message to the console indicating years for LTN calculation
print(paste("Calculate LTN over years: ", years[1], "-", years[2]))


# This line combines the dates of interest into a single vector
days <- c(date_start, date_end)


# Create chart title
chart_title <- paste0("Weekly climate chart for ", place_name, " ("
                      , lat, ", ", lon, ") "
                      , "\n", days[1], " to ", days[2])


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

dataToUse$current_weekly_precip <- zoo::rollapply(data = dataToUse$precipitationAccumulationSum,
               width = daysToAggregateOver,
               FUN = sum,
               align = "right",
               na.rm = TRUE,
               fill = NA,
               partial = TRUE)

# take every Nth row (7th for weekly stats)
dataToUse_current <- dataToUse[seq(from = daysToAggregateOver + 1
                           ,to = nrow(dataToUse)
                           ,by = daysToAggregateOver),]



# bar plot with weekly accumulated precip during CURRENT timeframe of interest
chart <- dataToUse_current %>%
  ggplot2::ggplot(aes(x = date,
                      y = current_weekly_precip)) +

  # display a point for each season statistic
  ggplot2::geom_col(fill = "#536878") +
  theme_bw() +
  labs(title = chart_title,
       x = "Date",
       y = "Weekly accumulated precipitation (mm)")






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
    dataToUse_LTN <- data.frame(month_day = dataToUse_past$month_day,
                                year_start = dataToUse_past$year_start,
                                week_count = dataToUse_past$week_count,
                                weekly_precip = dataToUse_past$past_weekly_precip)
  } else{
    dataToUse_LTN <- rbind(dataToUse_LTN,
                           data.frame(month_day = dataToUse_past$month_day,
                                      year_start = dataToUse_past$year_start,
                                      week_count = dataToUse_past$week_count,
                                      weekly_precip = dataToUse_past$past_weekly_precip))
  }

}

# reshape LTN weekly data long to wide, with columns renamed using year
dataToUse_LTN <- dataToUse_LTN %>%
  tidyr::pivot_wider(id_cols = week_count,
                     names_from = year_start,
                     values_from = weekly_precip)

# calculate LTN weekly precip by taking the average of
# weekly aggregated precip across all LTN years
dataToUse_LTN$weekly_precip_LTN <- rowMeans(dataToUse_LTN[,c(as.character(years[1]:years[2]))],
                                            na.rm=TRUE)


# add LTN precip data to climate chart as line




















# another approach: mimic the aWhere chart function here -------------------------------

# Modify the code within the aWhere function to expect the column provided
# in the TomorrowNow data set

# attempting to mimic function with relevant variables
generateClimateChart <- function(data
                                ,variable
                                ,variable_rightAxis = NULL
                                ,date_start = NULL
                                ,date_end = NULL
                                ,title = NULL
                                ,e_precip = FALSE
                                ,e_threshold = 35
                                ,doRoll = FALSE
                                ,rolling_window = 30
                                ,includeSTD = FALSE
                                ,mainGraphType = 'line'
                                ,daysToAggregateOver = NULL
                                ,yAxisLimits = NA
                                ,size_font_main_title = 16
                                ,size_font_axis_titles = 14
                                ,size_font_axis_labels = 12
                                ,size_font_legend_entries = 12
                                ,line_width = 1
                                ,annotationsWhichSide = 'left') {


  # because we are going to change the datastructure and it is a data.table we
  # will explicitly copy what is passed in so it doesn't violate user's scoping
  # expectations
  dataToUse <- data.table::as.data.table(data.table::copy(data))

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

  # temporally aggregate
  daysToAggregateOver <- 7
  typesOfColumns <- c('.amount','.average','.stdDev') ### these col names are present in the aWhere dataset but not tomorrowNow
  variablesToProcess <- unique(gsub(pattern = paste0(typesOfColumns,collapse = '|')
                                    ,replacement = ''
                                    ,x = colnames(dataToUse)))
  variablesToProcess <- setdiff(variablesToProcess
                                ,c('latitude','longitude','date','day'))

  #The logic here is that the accumulated columns are already calculated for
  #temporally subsetting and nothing needs to be done.  For variables that are
  #logically summed over time, do that for the .amount and .average columns
  #but the .stdDev column should have the mean taken.  For all other columns
  #take the mean
  for (x in 1:length(variablesToProcess)) {
    for (y in 1:length(typesOfColumns)) {

      currentColumn <- paste0(variablesToProcess[x],typesOfColumns[y])

      #Additional years can be added to the dataset but only the .amount column will be present
      if ((grepl(pattern = 'year|rolling'
                 ,x = currentColumn
                 ,ignore.case = TRUE) & typesOfColumns[y] %in% c('.average','.stdDev')) == TRUE) {
        next

      } else if (grepl(pattern = 'accumulated'
                       ,x = currentColumn
                       ,fixed = TRUE) == TRUE) {

        eval(parse(text = paste0('dataToUse[,',paste0(currentColumn,'.new'),' := ',currentColumn,']')))
      } else {
        eval(parse(text = paste0('dataToUse[,',paste0(currentColumn,'.new'),' := zoo::rollapply(',currentColumn,'
                                   ,width = daysToAggregateOver
                                   ,align = "right"
                                   ,FUN = ',returnAppropriateSummaryStatistic(variablesToProcess[x]),'
                                   ,na.rm = TRUE
                                   ,fill = NA
                                   ,partial = TRUE)',seasonNumber_str,']')))
      }
    }
  }



}

weekly_chart_test <- generateClimateChart(data = weather.df,
                                       variable = "precipitation",
                                       title = chart.title,
                                       daysToAggregateOver = 7,
                                       mainGraphType = 'bar')
