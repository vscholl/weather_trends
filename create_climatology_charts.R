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
day_start <- "2021-09-01"
day_end <- "2022-05-31"


# --------------------------------------------------------------------------

# create output directory if it does not already exist
if(!exists("outputs")){
  dir.create("outputs")}

# read data file (.csv) into a tibble.
df <- readr::read_csv(data_filename)


# Set the time span over which to calculate the long term normal (LTN):
# the start year of the daily data file up to and through the current year -2
years <- c(format(min(df$date), "%Y"), as.numeric(format(Sys.Date(), "%Y")) - 2)

# Print a message to the console indicating years for LTN calculation
print(paste("Calculate LTN over years: ", years[1], "-", years[2]))


# This line combines the dates of interest into a single vector
days <- c(day_start, day_end)


# Create chart title
chart_title <- paste0("Weekly climate chart for ", place_name, " ("
                      , lat, ", ", lon, ") "
                      , "\n", days[1], " to ", days[2])


dataToUse <- data.table::as.data.table(data.table::copy(df))

# Subset the period of time the user wants charted to make an accumulation of "CURRENT time"
# Subset the data if the user specifies
if (is.null(day_start) == FALSE) {
  dataToUse <- dataToUse[date >= as.Date(day_start)]
}
if (is.null(day_end) == FALSE) {
  dataToUse <- dataToUse[date <= as.Date(day_end)]
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
dataToUse <- dataToUse[seq(from = daysToAggregateOver + 1
                           ,to = nrow(dataToUse)
                           ,by = daysToAggregateOver),]



# bar plot with weekly accumulated precip during current timeframe
chart <- dataToUse %>%
  ggplot2::ggplot(aes(x = date,
                      y = current_weekly_precip)) +

  # display a point for each season statistic
  ggplot2::geom_col(fill = "#536878") +
  theme_bw() +
  labs(title = chart_title,
       x = "Date",
       y = "Weekly accumulated precipitation (mm)")






# Calculate long term normal precip and add column labeled LTN


# add LTN precip data to climate chart as line









# another approach: mimic the aWhere chart function here -------------------------------

# Modify the code within the aWhere function to expect the column provided
# in the TomorrowNow data set

# attempting to mimic function with relevant variables
generateClimateChart <- function(data
                                ,variable
                                ,variable_rightAxis = NULL
                                ,day_start = NULL
                                ,day_end = NULL
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
  if (is.null(day_start) == FALSE) {
    dataToUse <- dataToUse[date >= as.Date(day_start)]
  }
  if (is.null(day_end) == FALSE) {
    dataToUse <- dataToUse[date <= as.Date(day_end)]
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
