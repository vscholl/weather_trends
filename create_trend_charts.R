# Create trend charts

# This script reads a weather data file (.csv) and outputs a series of
# trend charts as image files (.png) along with a table (.csv) with
# the statistical values visualized in each chart.


# install / load required R packages --------------------------------------

library(tidyverse) # readr::read_csv
library(dplyr) # for data frame manipulation and grouping statistics

# User-defined inputs -----------------------------------------------------

# Specify the file (.csv) containing weather data.
data_filename <- "data/Kitui_-1.21_38.12_daily.csv"

# Specify the "season" or time period of interest using start/end date "MM-DD"
# and give the time period a name.
monthday_start <- "03-15"
monthday_end <- "10-01"
season_name <- "15 March - 01 October"


# --------------------------------------------------------------------------

# read data file (.csv) into a tibble.
 df <- readr::read_csv(data_filename)

# counter to keep track of which season.
counter <- 0

# add a column to count which season corresponds to each data observation
df$season_number <- NA

# for each year in the data file (2000 - 2022),
for(y in seq(2000, 2022)){

  # print current year
  print(y)
  # increment counter by +1
  counter <- counter + 1

  # set the start and end date. If the season spans multiple years,
  # defined here as when the ending month is less than the starting month
  # (i.e. 12-01 to 01-31. December (12) to January (01) of following year.
  # 01 < 12, which means the season extends into the next year.)
  # then add +1 to the end date year.
  date_start <- as.Date(paste0(y,"-",monthday_start))
  date_end <- as.Date(paste0(y,"-",monthday_end))
  if(as.numeric(strftime(date_end,"%m")) <
     as.numeric(strftime(date_start,"%m"))){
    date_end <- as.Date(paste0(y+1,"-",monthday_end))
  }

  # print the starting and ending dates
  print(paste("start date:", date_start,
              "end date:", date_end))

  # check if the season extends too far into the future.
  # the latest end date possible is the latest date in the csv file.
  if(date_end > max(as.Date(df$date))){
    print(paste0("invalid end date ", monthday_end, " for data in current year, ", y))

    break # break out of the loop.
  }

  # add season counter to rows with data in the current date range
  df$season_number[(df$date >= date_start) & (df$date <= date_end)] <-
    counter

}

# Calculate season statistics for each year and variable ---------------

# separate the date of each row into columns for year, month, and day
df_stats <- df %>%
  dplyr::mutate(date_copy = date) %>%
  tidyr::separate(col = date_copy, into = c("year", "month", "day"), sep = "-")

# add a column for the starting year and month within each season
df_stats <- df_stats %>%
  dplyr::group_by(season_number) %>%
  dplyr::mutate(season_start_year = min(year))

# calculate statistics for each season:
# monthday_start to monthday_end for every year in the data record.
n_seasons <- length(unique(df_stats$season_number))
season_stats <- df_stats %>%
  dplyr::group_by(season_number, year = season_start_year) %>%
  dplyr::summarise(precip = sum(precipitation.amount),
                   minT = mean(temperatures.min.amount),
                   maxT = mean(temperatures.max.amount),
                   #ppet = mean(ppet.amount)
                   )
