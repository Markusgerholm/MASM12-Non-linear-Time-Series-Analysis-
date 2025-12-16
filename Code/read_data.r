## Libraries
library(readr)
library(dplyr)
## Lund - soldata
lund <- readr::read_csv("Data/lund.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Helsingborg
helsingborg <- readr::read_csv("Data/helsingborg.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Nidingen
nidingen <- readr::read_csv("Data/nidingen.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Falsterbo
falsterbo <- readr::read_csv("Data/falsterbo.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Hallands Väderö
halland <- readr::read_csv("Data/halland.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Hörby
hörby <- readr::read_csv("Data/hörby.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Malmö
malmö <- readr::read_csv("Data/malmö.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))
