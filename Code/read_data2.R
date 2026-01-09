## Libraries
library(readr)
library(dplyr)

## Helsingborg
helsingborg <- readr::read_csv("Data/helsingborg.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

helsingborg_2024 <- helsingborg %>%
  filter(
    ts_utc >= as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ts_utc <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
  )
start <- 3200
end <- 5900
df_he <- helsingborg_2024[start:end, ] # center
## Hörby
hörby <- readr::read_csv("Data/hörby.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

hörby_2024 <- hörby %>%
  filter(
    ts_utc >= as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ts_utc <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
  )

df_hö <- hörby_2024[start:end, ] # center

## Ullared
ullared <- readr::read_csv("Data/ullared.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

ullared_2024 <- ullared %>%
  filter(
    ts_utc >= as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ts_utc <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
  )

df_ul <- ullared_2024[start:end, ] # center

## Falsterbo
falsterbo <- readr::read_csv("Data/falsterbo.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

falsterbo_2024 <- falsterbo %>%
  filter(
    ts_utc >= as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ts_utc <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
  )

df_fa <- falsterbo_2024[start:end, ]
## Danish data
params <- c("temp_dry","wind_dir","wind_speed")

start_time <- format(as.POSIXct(df_he$ts_utc[1], tz = "UTC"),
                     "%Y-%m-%dT%H:%M:%SZ")
end_time <- format(as.POSIXct(df_he$ts_utc[nrow(df_he)], tz = "UTC"),
                   "%Y-%m-%dT%H:%M:%SZ")
stations <- c(
  kb = "06180", # Kobenhavn lufthavn
  ro = "06170", # Roskilde Lufthavn
  gn = "06049", # Gniben
  sl = "06073", # Sletterhage Fyr
  an = "06079"  # Anholt
)

df_list <- vector("list", length(stations))
names(df_list) <- names(stations)

missing_list <- vector("list", length(stations))
names(missing_list) <- names(stations)

for (nm in names(stations)) {
  station_id <- stations[[nm]]
  
  observations <- get_observations_for_station(
    station_id = station_id,
    parameters = params,
    start_dt   = start_time,
    end_dt     = end_time,
    limit      = 20000
  )
  
  full_seq <- seq(
    from = min(observations$observed),
    to   = max(observations$observed),
    by   = "10 min"
  )
  
  missing_times <- full_seq[!full_seq %in% observations$observed]
  missing_list[[nm]] <- missing_times
  
  df <- process_hourly_data(observations) %>%
    rename(
      Lufttemperatur = temp_dry,
      Vindriktning   = wind_dir,
      Vindhastighet  = wind_speed,
      ts_utc         = observed
    )
  df_list[[nm]] <- df
}

df_kb <- df_list$kb
df_ro <- df_list$ro
df_gn <- df_list$gn
df_sl <- df_list$sl
df_an <- df_list$an