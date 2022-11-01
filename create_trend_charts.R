# Create trend charts

# This script reads a weather data file (.csv) and outputs a series of
# trend charts as image files (.png) along with a table (.csv) with
# the statistical values visualized in each chart.


# install / load required R packages --------------------------------------

library(tidyverse) # includes packages such as readr, dplyr, tidyr


# User-defined inputs -----------------------------------------------------

# Specify the file (.csv) containing weather data.
data_filename <- "data/Kitui_-1.21_38.12_daily.csv"

# Specify the place name, latitide, and longitude coordinates
lat <- "-1.21"
lon <- "38.12"
place_name <- "Kitui, Kenya"

# Specify the "season" or time period of interest using start/end date "MM-DD"
# and give the time period a name.
monthday_start <- "10-15"
monthday_end <- "11-15"
season_name <- "15 October - 15 November"

# Add regression line to charts: TRUE or FALSE
add_regression_line <- TRUE


# --------------------------------------------------------------------------

# create output directory if it does not already exist
if(!exists("outputs")){
  dir.create("outputs")}

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

# separate the date of each row into columns for year, month, and day.
df_stats <- df %>%
  dplyr::mutate(date_copy = date) %>%
  tidyr::separate(col = date_copy, into = c("year", "month", "day"),
                  sep = "-") %>%
  # remove rows with NA values in season_number column
  dplyr::filter(!is.na(season_number))

# add a column for the starting year and month within each season.
df_stats <- df_stats %>%
  dplyr::group_by(season_number) %>%
  dplyr::mutate(season_start_year = min(year))

# calculate statistics for each season:
# monthday_start to monthday_end for every year in the data record.
n_seasons <- length(unique(df_stats$season_number))
season_stats <- df_stats %>%
  dplyr::group_by(season_number, year = season_start_year) %>%
                   # sum the rainfall values between the two dates
  dplyr::summarise(precip = sum(totalPrecipitationAccumulation),
                   # mean temperature extremes between the two dates
                   minT = mean(temperatureMin),
                   maxT = mean(temperatureMax)
                   #,ppet = mean(ppet.amount) # VS TO DO WHEN P/PET DATA ARE AVAILABLE
                   )

# Create charts to show season statistics --------------------------------

# set the appropriate weather variable title and units
for(weather_var in c("precip", "minT", "maxT"
                     #, "ppet" # VS TO DO WHEN P/PET DATA ARE AVAILABLE
                     )){

  if(weather_var == "precip"){
    var_title <- "precipitation"
    var_units <- "[mm]"
  } else if(weather_var == "minT"){
    var_title <- "minimum tteperature"
    var_units <- "[deg C]"
  } else if(weather_var == "maxT"){
    var_title <- "maximum teperature"
    var_units <- "[deg C]"
  } else if(weather_var == "ppet"){
    var_title <- "P/PET"
    var_units <- ""
  }

  # calculate mean, standard deviation, CV, P-value for current weather var
  var_values <- as.data.frame(season_stats[, as.character(weather_var)])

  # print status message to the console
  print(paste0("Creating ", var_title, " chart..."))

  # start creating the current chart
  current_chart <- season_stats %>%
    ggplot2::ggplot(aes(x = year,
                        y = get(weather_var),
                        group = 1)) +

    # display a point for each season statistic
    ggplot2::geom_point() +

    # connect the data points with a line
    ggplot2::geom_line(linetype = "solid") +

    # clean plot theme with white background
    ggplot2::theme_bw() +

    # set the font size and rotation of axis labels.
    # place the legend on the bottom of the chart.
    theme(axis.text.x = element_text(size = 14, angle=45, hjust=1),
          axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 14, hjust=1),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14))

  # Optionally add linear regression line
  if(add_regression_line == TRUE){

    # Calculate a linear model to estimate precipitation as a function of year
    var_lm <- lm(get(weather_var)~as.numeric(year), season_stats)

    # Print a summary of the linear regression coefficients and other metrics
    summary(var_lm)

    # Extract P-values of the linear regression
    var_lm_P_values <- coefficients(summary(var_lm))[,4]
    # Extract T-test values of the linear regression
    var_lm_t_values <- coefficients(summary(var_lm))[,3]

    # Add linear regression line to the chart
    current_chart <- current_chart +
      ggplot2::geom_smooth(method = "lm", se = FALSE,
                           color = "black", linetype = "dashed")
  }

  # format the title, subtitle, and axis labels
  current_chart <- current_chart +
    ggplot2::labs(x = "year",
                  y = paste("seasonal", var_title, var_units, "\n"),


                  #title = paste0("Annual ", var_title, " variability for ",
                    #            season_name, ", ", place_name, " (", lat, ", ", lon, ")"),

                  title = paste0("Annual ", var_title, " variability in ",
                                 place_name, " (", lat, ", ", lon, ")",
                                 " during season: ", season_name),

                  subtitle = paste0("mean ", var_title, " = ",
                                   round((mean(var_values[,1])),2),
                                   ", CV = ",
                                   round(((sd(var_values[,1])) /
                                            (mean(var_values[,1]))*100),0),
                                   "%, SD = ", round((sd(var_values[,1])),2),
                                   ", P-value = ", round(var_lm_P_values[[2]], digits = 3)))

  # display the chart
  #current_chart

  # write current chart to image file
  ggplot2::ggsave(filename = paste0("outputs/", place_name, "_seasonal_",
                                    weather_var,"_chart_",
                                    stringr::str_replace_all(season_name, " ", ""),
                                    "_",lat, "_",lon, ".png"),
                  plot = current_chart,
                  width = 12.0, height = 6, units = "in", device = "png")


} # end chart creation loop over weather variables

