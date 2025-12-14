library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)

## Wind direction 
labels16 <- c("N","NNE","NE","ENE","E","ESE","SE","SSE",
              "S","SSW","SW","WSW","W","WNW","NW","NNW")

df_dir <- helsingborg4 %>%
  filter(!is.na(Vindriktning)) %>%
  mutate(
    wd = Vindriktning %% 360,
    dir16 = labels16[(floor((wd + 11.25) / 22.5) %% 16) + 1]
  ) %>%
  count(dir16)

# Wind-rose style (same counts, circular)
ggplot(df_dir, aes(x = dir16, y = n)) +
  geom_col() +
  coord_polar(start = -0.2) +   # N at top
  labs(x = NULL, y = "Count") + scale_x_discrete(limits = c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                                                            "S","SSW","SW","WSW","W","WNW","NW","NNW"))

## Checking why data have differing lengths

# Helper: extract unique UTC timestamps from a dataset
get_ts_utc <- function(df) {
  df %>%
    mutate(
      ts_utc = parse_date_time(
        paste(Datum, Tid..UTC.),
        orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM"),
        tz = "UTC"
      )
    ) %>%
    filter(!is.na(ts_utc)) %>%
    distinct(ts_utc) %>%
    pull(ts_utc)
}

ts_list <- list(
  temp   = get_ts_utc(helsingborg1),
  rh     = get_ts_utc(helsingborg2),
  precip = get_ts_utc(helsingborg3),
  wind   = get_ts_utc(helsingborg4)
)

# Expected hourly sequence (UTC) for 2021-01 through 2025-08
ref <- seq(
  from = as.POSIXct("2021-01-01 00:00:00", tz = "UTC"),
  to   = as.POSIXct("2025-08-31 23:00:00", tz = "UTC"),
  by   = "hour"
)

# Presence table: one row per hour, TRUE/FALSE per dataset
presence <- ts_list %>%
  imap(~ tibble(ts_utc = .x, !!.y := TRUE)) %>%
  reduce(full_join, by = "ts_utc") %>%
  right_join(tibble(ts_utc = ref), by = "ts_utc") %>%
  mutate(across(-ts_utc, ~ replace_na(.x, FALSE)))

# All hours missing in at least one dataset (+ which ones)
missing_between_datasets <- presence %>%
  filter(if_any(-ts_utc, ~ .x == FALSE)) %>%
  arrange(ts_utc)

missing_between_datasets

## Summary of the sum of missing timestamps

missing_summary <- presence %>%
  summarise(across(-ts_utc, ~ sum(.x == FALSE)))

missing_summary

## Creating master data frame for Helsingborg where missing time stamps are NA
make_ts <- function(df) {
  df %>% mutate(ts_utc = as.POSIXct(paste(Datum, Tid..UTC.), tz = "UTC"))
}

temp   <- make_ts(helsingborg1) %>% select(ts_utc, Lufttemperatur)
rh     <- make_ts(helsingborg2) %>% select(ts_utc, Relativ.Luftfuktighet)
precip <- make_ts(helsingborg3) %>% select(ts_utc, Nederbördsmängd)
wind_direction   <- make_ts(helsingborg4) %>% select(ts_utc, Vindriktning)
wind_speed <- make_ts(helsingborg4) %>% select(ts_utc, Vindhastighet)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")

helsingborg <- tibble(ts_utc = ref) %>%
  left_join(temp,   by="ts_utc") %>%
  left_join(rh,     by="ts_utc") %>%
  left_join(precip, by="ts_utc") %>%
  left_join(wind_direction,   by="ts_utc") %>%
  left_join(wind_speed, by="ts_utc")

