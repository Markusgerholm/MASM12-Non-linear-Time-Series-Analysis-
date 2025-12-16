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

## Hörby
hörby <- readr::read_csv("Data/hörby.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Skillinge
skillinge <- readr::read_csv("Data/skillinge.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Hästveda
hästveda <- readr::read_csv("Data/hästveda.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Ullared
ullared <- readr::read_csv("Data/ullared.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))

## Ljungby
ljungby <- readr::read_csv("Data/ljungby.csv") %>%
  mutate(ts_utc = as.POSIXct(ts_utc, tz = "UTC"))
