## Ängelholm

angelholm <- read.csv("Data/observation_angelholm-barkakra_flygplats_core_62180_last4y_20251208200623.csv")
parameterkey <- read.csv("Data/parameter_20251203170108.csv")
library(dplyr)
library(tidyr)
library(lubridate)

most_unique <- angelholm %>%
  group_by(parameter, date, time) %>%
  summarise(
    n_unique = n_distinct(value, na.rm = TRUE),
    values   = paste(sort(unique(value)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_unique > 1) %>%
  slice_max(n_unique, n = 1, with_ties = TRUE)

most_unique

p <- 1

max_n_rows_dt_for_p <- angelholm %>%
  filter(parameter == p) %>%
  count(date, time, name = "n_rows") %>%
  summarise(max_n_rows = max(n_rows)) %>%
  pull(max_n_rows)

max_n_rows_dt_for_p

## CONCLUSION: Average over hour, only four hours in total where hourly observed value differ CHECK WITH SMHI

## Falsterbo

parameterkey <- read.csv("Data/parameter_20251203170108.csv")
library(dplyr)
library(tidyr)
library(lubridate)
falsterbo <- read.csv("Data/observation_falsterbo_a_regionalt_52240_last4y_20251208195817.csv")

most_unique <- falsterbo %>%
  group_by(parameter, date, time) %>%
  summarise(
    n_unique = n_distinct(value, na.rm = TRUE),
    values   = paste(sort(unique(value)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_unique > 1) %>%
  slice_max(n_unique, n = 1, with_ties = TRUE)

most_unique
 
## CONCLUSUON: All hourly values are the same!

## Hallands Vädero
parameterkey <- read.csv("Data/parameter_20251203170108.csv")
library(dplyr)
library(tidyr)
library(lubridate)
halland <- read.csv("Data/observation_hallands_vadero_a_regionalt_62260_last4y_20251208200645.csv")
unique(halland$parameter)
most_unique <- halland %>%
  group_by(parameter, date, time) %>%
  summarise(
    n_unique = n_distinct(value, na.rm = TRUE),
    values   = paste(sort(unique(value)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_unique > 1) %>%
  slice_max(n_unique, n = 1, with_ties = TRUE)

most_unique

## CONCLUSiON: Missing parameters -> contact ÖKAB, only parameter 16 is in data, we need: 1, 3, 4, 6, 7, 9

## Helsingborg

parameterkey <- read.csv("Data/parameter_20251203170108.csv")
library(dplyr)
library(tidyr)
library(lubridate)
helsingborg <- read.csv("Data/observation_helsingborg_a_core_62040_last4y_20251208200538.csv")

most_unique <- helsingborg %>%
  group_by(parameter, date, time) %>%
  summarise(
    n_unique = n_distinct(value, na.rm = TRUE),
    values   = paste(sort(unique(value)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_unique > 1) %>%
  slice_max(n_unique, n = 1, with_ties = TRUE)

most_unique

helsing_mini <- helsingborg %>%
  filter(date == as.Date("2025-01-01"))## CONCLUSUON: All hourly values are the same!

## Lund

parameterkey <- read.csv("Data/parameter_20251203170108.csv")
library(dplyr)
library(tidyr)
library(lubridate)
lund <- read.csv("Data/observation_lund_sol_regionalt_53445_last4y_20251208195801.csv")
unique(lund$parameter)
most_unique <- helsingborg %>%
  group_by(parameter, date, time) %>%
  summarise(
    n_unique = n_distinct(value, na.rm = TRUE),
    values   = paste(sort(unique(value)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_unique > 1) %>%
  slice_max(n_unique, n = 1, with_ties = TRUE)

most_unique

## Conclusion: Change to Hörby - no values of temperature etc

## Parameters we might want

## Global irradians och solskenstid - Lund
soldata <- read.csv("Data/smhi-opendata_10_53445_202301_202508.csv",skip=9, sep = ";")

# NOTE: time is in UTC, check Ängelholm dupes with SMHI website, Hallands Väderö missing parameters, Change Lund to Hörby

## Parameters to download

# 1: Lufttemperatur (h)
# 3: Vindriktning
# 4: Vindhastighet
# 6: Relativ luftfuktighet
# 7: Nederbördsmängd (h)