## Libraries
library(readr)
library(dplyr)

## Helsingborg
helsingborg <- readr::read_csv("Data/helsingborg.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Hörby
hörby <- readr::read_csv("Data/hörby.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Ullared
ullared <- readr::read_csv("Data/ullared.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

