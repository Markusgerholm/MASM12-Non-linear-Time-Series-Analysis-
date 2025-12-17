library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

get_stations <- function(limit = 1000, offset = 0) {
  stations_url <- "https://opendataapi.dmi.dk/v2/metObs/collections/station/items"
  resp <- GET(stations_url, query = list(limit = limit, offset = offset))
  stop_for_status(resp)
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  stations <- data$features$properties
  return(stations)
}

get_station_available_parameters <- function(station_id, limit = 1000, offset = 0) {
  stations <- get_stations(limit = limit, offset = offset)
  
  # Filter for the desired station
  station_row <- stations %>% filter(stationId == station_id)
  if (nrow(station_row) == 0) {
    warning("Station ID not found")
    return(NULL)
  }
  
  # Extract parameterId column
  param_string <- station_row$parameterId[1]
  
  # Remove the "c(" and ")" and quotes, then split by comma
  param_clean <- param_string %>%
    str_remove_all("^c\\(|\\)$") %>%   # remove leading c( and trailing )
    str_remove_all("\"") %>%           # remove quotes
    str_split(",") %>%
    unlist() %>%
    trimws()
  
  unique(param_clean)
}


get_observations_for_station <- function(
    station_id, 
    parameters, 
    start_dt, 
    end_dt, 
    limit = 10000,
    frequency = NULL   # e.g., "hourly", "10 min", or NULL
) {
  obs_url <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items"
  
  datetime_range <- paste0(start_dt, "/", end_dt)
  all_obs <- list()
  
  for (param in parameters) {
    query <- list(
      datetime    = datetime_range,
      stationId   = station_id,
      parameterId = param,
      limit       = limit,
      sortorder   = "observed,DESC"
    )
    
    resp <- GET(obs_url, query = query)
    stop_for_status(resp)
    
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if (length(data$features) == 0) {
      warning(paste("No observations found for parameter:", param))
      next
    }
    
    obs_df <- data$features$properties %>%
      select(observed, value) %>%
      rename(!!param := value) %>%
      mutate(observed = as.POSIXct(observed, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
    
    all_obs[[param]] <- obs_df
  }
  
  if (length(all_obs) == 0) return(NULL)
  
  # Merge all observations by timestamp
  tidy_obs <- reduce(all_obs, full_join, by = "observed") %>%
    arrange(observed)
  
  # Fill in missing timestamps if frequency is specified
  if (!is.null(frequency)) {
    # Create sequence of timestamps
    ts_seq <- seq(from = min(tidy_obs$observed), to = max(tidy_obs$observed), by = frequency)
    
    tidy_obs <- tidy_obs %>%
      right_join(data.frame(observed = ts_seq), by = "observed") %>%
      arrange(observed)
  }
  
  return(tidy_obs)
}
