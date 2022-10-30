# Create trend charts

# This script reads a weather data file (.csv) and outputs a series of
# trend charts as image files (.png) along with a table (.csv) with
# the statistical values visualized in each chart.


# install / load required R packages --------------------------------------

library(tidyverse) # readr
library(dplyr)

# User-defined inputs -----------------------------------------------------

# Specify the file (.csv) containing weather data.
data_filename <- "data/Kitui_-1.21_38.12_daily.csv"

# Specify the "season" or time period of interest using start/end date "MM-DD"
# and give the time period a name.
monthday_start <- "03-15"
monthday_end <- "10-10"
season_name <- "15 March - 10 October"


# Create charts -----------------------------------------------------------

# Read data file (.csv) into a tibble
weather_dt <- readr::read_csv(data_filename)

