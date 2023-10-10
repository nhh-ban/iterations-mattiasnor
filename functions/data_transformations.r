## Imports -----------
library(tidyverse)
library(purrr)
library(lubridate)
library(anytime)
library(tibble)


## Problem 3 -----------
transform_metadata_to_df <- function(stations_metadata) {
  stations_df <- stations_metadata[[1]] %>% 
    map_df(function(x) {
      tibble(
        id = x$id, 
        name = x$name, 
        latestData = as_datetime(x$latestData$volumeByHour, tz = "Europe/Berlin"),
        lat = x$location$coordinates$latLon$lat, 
        lon = x$location$coordinates$latLon$lon
      )
    })
  
  # Convert latestData to UTC format
  stations_df$latestData <- with_tz(stations_df$latestData, "UTC")
  
  return(stations_df)
}



## Problem 4 -----------

to_iso8601 <- function(datetime, offset) {
  #Add the offset
  adjusted_datetime <- datetime + days(offset)
  
  #Adjust the timezone to UTC
  adjusted_datetime <- with_tz(adjusted_datetime, "UTC")
  
  iso_date <- anytime::iso8601(adjusted_datetime)
  
  return(paste0(iso_date, "Z"))
}


## Problem 5 -----------

transform_volumes <- function(json_data) {
  #Extract the edges from the json_data
  edges <- json_data$trafficData$volume$buHour$edges
  
  #Using map_df to extract the data and convert to a dataframe
  df <- map_df(edges, function(edge) {
    tibble(
      from = as_datetime(edge$node$from), 
      to = as_datetime(edge$node$to), 
      volume = edge$node$total$volumeNumbers$volume
    )
  })
  
  return(df)
}

