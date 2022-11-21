# Create climatology charts

# This script reads a weather data file (.csv) and outputs a
# climatology chart visualizing current and long-term normal
# weekly rainfall.


# install / load required R packages --------------------------------------

library(tidyverse) # includes packages such as readr, dplyr, tidyr


# User-defined inputs -----------------------------------------------------

# Specify the file (.csv) containing weather data.
data_filename <- "data/Kenya Kibomet_1.06_35.03_daily-1.csv"

# Specify the place name, latitude, and longitude coordinates
lat <- "1.06"
lon <- "35.03"
place_name <- "Kibomet, Kenya"

# Specify dates of interest using YYYY-MM-DD format
day.start <- "2021-09-01"
day.end <- "2022-05-31"


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
days <- c(day.start, day.end)


# Create chart title
chart.title <- paste0("Weekly Climate Chart for ", place_name, "\n      Latitude "
                      , lat,",  Longitude ", lon
                      , "       ", days[1], " to ", days[2])


# Calculate LTN for precip


# Subset the period of time the user wants charted to make an accumulation of "current time"


# Create weekly precipitation chart


