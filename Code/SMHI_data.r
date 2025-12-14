## Soldata från Lund
soldata <- read.csv("Data/sol_lund.csv", sep = ";")

## Helsingborg
helsingborg1 <- read.csv("Data/temp_helsingborg.csv", sep = ";")
helsingborg2 <- read.csv("Data/relativluftfuktighet_helsingborg.csv", sep = ";")
helsingborg3 <- read.csv("Data/nederbörd_helsingborg.csv", sep = ";")
helsingborg4 <- read.csv("Data/vind_helsingborg.csv", sep = ";")

## Falsterbo
falsterbo1 <- read.csv("Data/temp_falsterbo.csv", sep = ";")
falsterbo2 <- read.csv("Data/relativluftfuktighet_falsterbo.csv", sep = ";")
falsterbo3 <- read.csv("Data/nederbörd_falsterbo.csv", sep = ";")
falsterbo4 <- read.csv("Data/vind_falsterbo.csv", sep = ";")

## Ängelholm
ängelholm1 <- read.csv("Data/temp_ängelholm.csv", sep = ";")
ängelholm2 <- read.csv("Data/relativluftfuktighet_ängelholm.csv", sep = ";")
# nederbörd FANNS EJ: ängelholm3 <- read.csv("Data/", sep = ";")
ängelholm4 <- read.csv("Data/vind_ängelholm.csv", sep = ";")

## Hallands Väderö
halland1 <- read.csv("Data/temp_halland.csv", sep = ";")
halland2 <- read.csv("Data/relativluftfuktighet_halland.csv", sep = ";")
halland3 <- read.csv("Data/nederbörd_halland.csv", sep = ";")
halland4 <- read.csv("Data/vind_halland.csv", sep = ";")

## Hörby
hörby1 <- read.csv("Data/temp_hörby.csv", sep = ";")
hörby2 <- read.csv("Data/relativluftfuktighet_hörby.csv", sep = ";")
hörby3 <- read.csv("Data/nederbörd_hörby.csv", sep = ";")
hörby4 <- read.csv("Data/vind_hörby.csv", sep = ";")

## Malmö
malmö1 <- read.csv("Data/temp_malmö.csv", sep = ";")
malmö2 <- read.csv("Data/relativluftfuktighet_malmö.csv", sep = ";")
malmö3 <- read.csv("Data/nederbörd_malmö.csv", sep = ";")
malmö4 <- read.csv("Data/vind_malmö.csv", sep = ";")



## Vindriktning koll
library(dplyr)
library(ggplot2)

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

