## Libraries
library(readr)

## Function for creating time series
make_ts <- function(df) {
  df %>% mutate(ts_utc = as.POSIXct(paste(Datum, Tid..UTC.), tz = "UTC"))
}

## Soldata från Lund
lund1 <- read.csv("Data/sol_lund.csv", sep = ";")
global_radiation   <- make_ts(lund1) %>% select(ts_utc, Global.Irradians..svenska.stationer.)
sunshine_duration <- make_ts(lund1) %>% select(ts_utc, Solskenstid)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")
lund <- tibble(ts_utc = ref) %>% 
  left_join(global_radiation, by="ts_utc") %>%
  left_join(sunshine_duration, by="ts_utc")

## Saving csv file
#write.csv(lund, "lund.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Helsingborg
helsingborg1 <- read.csv("Data/temp_helsingborg.csv", sep = ";")
helsingborg2 <- read.csv("Data/relativluftfuktighet_helsingborg.csv", sep = ";")
helsingborg3 <- read.csv("Data/nederbörd_helsingborg.csv", sep = ";")
helsingborg4 <- read.csv("Data/vind_helsingborg.csv", sep = ";")

## Creating master data frame for Helsingborg where missing time stamps are NA
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

## Saving csv file
#write.csv(helsingborg, "helsingborg.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Falsterbo
falsterbo1 <- read.csv("Data/temp_falsterbo.csv", sep = ";")
falsterbo2 <- read.csv("Data/relativluftfuktighet_falsterbo.csv", sep = ";")
falsterbo3 <- read.csv("Data/nederbörd_falsterbo.csv", sep = ";")
falsterbo4 <- read.csv("Data/vind_falsterbo.csv", sep = ";")

## Creating master data frame for Falsterbo where missing time stamps are NA
temp   <- make_ts(falsterbo1) %>% select(ts_utc, Lufttemperatur)
rh     <- make_ts(falsterbo2) %>% select(ts_utc, Relativ.Luftfuktighet)
precip <- make_ts(falsterbo3) %>% select(ts_utc, Nederbördsmängd)
wind_direction   <- make_ts(falsterbo4) %>% select(ts_utc, Vindriktning)
wind_speed <- make_ts(falsterbo4) %>% select(ts_utc, Vindhastighet)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")

falsterbo <- tibble(ts_utc = ref) %>%
  left_join(temp,   by="ts_utc") %>%
  left_join(rh,     by="ts_utc") %>%
  left_join(precip, by="ts_utc") %>%
  left_join(wind_direction,   by="ts_utc") %>%
  left_join(wind_speed, by="ts_utc")

## Saving csv file
#write.csv(falsterbo, "falsterbo.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Nidingen
nidingen1 <- read.csv("Data/nidingen_temp.csv", sep = ";")
nidingen4 <- read.csv("Data/nidingen_vind.csv", sep = ";")


temp   <- make_ts(nidingen1) %>% select(ts_utc, Lufttemperatur)
#rh     <- make_ts(halland2) %>% select(ts_utc, Relativ.Luftfuktighet)
#precip <- make_ts(halland3) %>% select(ts_utc, Nederbördsmängd)
wind_direction   <- make_ts(nidingen4) %>% select(ts_utc, Vindriktning)
wind_speed <- make_ts(nidingen4) %>% select(ts_utc, Vindhastighet)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")

nidingen <- tibble(ts_utc = ref) %>%
  left_join(temp,   by="ts_utc") %>%
  #left_join(rh,     by="ts_utc") %>%
  #left_join(precip, by="ts_utc") %>%
  left_join(wind_direction,   by="ts_utc") %>%
  left_join(wind_speed, by="ts_utc")
#write.csv(nidingen, "nidingen.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Saving csv file
#write.csv(ängelholm, "ängelholm.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Hallands Väderö
halland1 <- read.csv("Data/temp_halland.csv", sep = ";")
halland2 <- read.csv("Data/relativluftfuktighet_halland.csv", sep = ";")
halland3 <- read.csv("Data/nederbörd_halland.csv", sep = ";")
halland4 <- read.csv("Data/vind_halland.csv", sep = ";")

## Creating master data frame for Halland where missing time stamps are NA
temp   <- make_ts(halland1) %>% select(ts_utc, Lufttemperatur)
rh     <- make_ts(halland2) %>% select(ts_utc, Relativ.Luftfuktighet)
precip <- make_ts(halland3) %>% select(ts_utc, Nederbördsmängd)
wind_direction   <- make_ts(halland4) %>% select(ts_utc, Vindriktning)
wind_speed <- make_ts(halland4) %>% select(ts_utc, Vindhastighet)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")

halland <- tibble(ts_utc = ref) %>%
  left_join(temp,   by="ts_utc") %>%
  left_join(rh,     by="ts_utc") %>%
  left_join(precip, by="ts_utc") %>%
  left_join(wind_direction,   by="ts_utc") %>%
  left_join(wind_speed, by="ts_utc")

## Saving csv file
#write.csv(halland, "halland.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Hörby
hörby1 <- read.csv("Data/temp_hörby.csv", sep = ";")
hörby2 <- read.csv("Data/relativluftfuktighet_hörby.csv", sep = ";")
hörby3 <- read.csv("Data/nederbörd_hörby.csv", sep = ";")
hörby4 <- read.csv("Data/vind_hörby.csv", sep = ";")

## Creating master data frame for Falsterbo where missing time stamps are NA
temp   <- make_ts(hörby1) %>% select(ts_utc, Lufttemperatur)
rh     <- make_ts(hörby2) %>% select(ts_utc, Relativ.Luftfuktighet)
precip <- make_ts(hörby3) %>% select(ts_utc, Nederbördsmängd)
wind_direction   <- make_ts(hörby4) %>% select(ts_utc, Vindriktning)
wind_speed <- make_ts(hörby4) %>% select(ts_utc, Vindhastighet)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")

hörby <- tibble(ts_utc = ref) %>%
  left_join(temp,   by="ts_utc") %>%
  left_join(rh,     by="ts_utc") %>%
  left_join(precip, by="ts_utc") %>%
  left_join(wind_direction,   by="ts_utc") %>%
  left_join(wind_speed, by="ts_utc")

## Saving csv file
#write.csv(hörby, "hörby.csv", row.names = FALSE, fileEncoding = "UTF-8")


## Malmö
malmö1 <- read.csv("Data/temp_malmö.csv", sep = ";")
malmö2 <- read.csv("Data/relativluftfuktighet_malmö.csv", sep = ";")
malmö3 <- read.csv("Data/nederbörd_malmö.csv", sep = ";")
malmö4 <- read.csv("Data/vind_malmö.csv", sep = ";")

## Creating master data frame for Falsterbo where missing time stamps are NA
temp   <- make_ts(malmö1) %>% select(ts_utc, Lufttemperatur)
rh     <- make_ts(malmö2) %>% select(ts_utc, Relativ.Luftfuktighet)
precip <- make_ts(malmö3) %>% select(ts_utc, Nederbördsmängd)
wind_direction   <- make_ts(malmö4) %>% select(ts_utc, Vindriktning)
wind_speed <- make_ts(malmö4) %>% select(ts_utc, Vindhastighet)
ref <- seq(as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
           as.POSIXct("2025-08-31 23:00:00", tz="UTC"),
           by="hour")

malmö <- tibble(ts_utc = ref) %>%
  left_join(temp,   by="ts_utc") %>%
  left_join(rh,     by="ts_utc") %>%
  left_join(precip, by="ts_utc") %>%
  left_join(wind_direction,   by="ts_utc") %>%
  left_join(wind_speed, by="ts_utc")

## Saving csv file
#write.csv(malmö, "malmö.csv", row.names = FALSE, fileEncoding = "UTF-8")

