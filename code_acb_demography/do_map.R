# Load needed packages:
library(readr)
library(countrycode)
library(dplyr)
library(rworldmap)
library(maps)

# Set working directory:
setwd("")

# Load world data:
data(world.cities) # ?world.cities
# View the list of countries:
View(sort(unique(world.cities$country.etc)))

# Load 2020-2021 players' data: 
df0 <- read_csv("code_acb_demography/output/2020_2021/players_data_2021.csv")

# There is one player with no scraped information:
# http://www.acb.com/jugador/temporada-a-temporada/id/30000998
df0[is.na(df0$Nationality), "Nationality"] <- "Dinamarca"

nat_spa <- sort(unique(df0$Nationality))
nat_eng_tr <- countryname(nat_spa)

nat_spa[is.na(nat_eng_tr)]
nat_eng_tr[is.na(nat_eng_tr)] <- c("Belarus", "USA", "UK", "Czech Republic", 
                                   "Dominican Republic", "Sudan")

df1 <- df0 %>%
  mutate(Nationality = plyr::mapvalues(Nationality, 
                                       from = nat_spa,
                                       to = nat_eng_tr))
# Quino Colom is an Andorran player who has always played for Spain.
df1$Nationality[df1$Player == "J. Colom"] <- "Spain"

# Number of different nationalities in the league:
length(unique(df1$Nationality))
#[1] 56

df2 <- df1 %>%
  count(Nationality) %>%
  rename(country = 1, value = 2) 

# Differences of names between the players' data and the standard:
countries_diff <- setdiff(df2$country, world.cities$country.etc)
countries_diff
df2 %>%
  filter(country %in% c("Serbia", "Montenegro"))

# Serbia and Montenegro still go together as Serbia and Montenegro in world.cities$country,
# but not in joinCountryData2Map, so we will have to add the coordinates of their capitals manually.
df2[df2$country %in% countries_diff, "country"] <- c("Bosnia and Herzegovina", 
                                                     "Congo", "Congo Democratic Republic",
                                                     "Montenegro", "Macedonia", "Serbia",
                                                     "Saint Kitts and Nevis")

df3 <- df2 %>%
  arrange(country) %>%
  mutate(continent = countrycode(country, origin = "country.name", destination = "region"))
df3$continent[df3$country == "Australia"] <- "Oceania"

df3 %>%
  arrange(-value) %>% 
  slice_max(value, n = 3)

df3 %>%
  mutate(label = ifelse(country == "Spain", "spaniard", "foreigner")) %>%
  group_by(label) %>%
  summarise(num_players = sum(value)) %>%
  ungroup() %>%
  mutate(num_players_perc = round((num_players / nrow(df1)) * 100, 1))

caps <- world.cities %>%
  filter(country.etc %in% df3$country, capital == 1) %>%
  rename(country = country.etc) %>%
  select(country, lat, long) %>%
  arrange(country)

caps1 <- left_join(caps, df3, by = "country")
serbia_and_montenegro <- data.frame(country = c("Serbia", "Montenegro"),
                                    lat = c(44.8, 42.4),
                                    long = c(20.5, 19.2),
                                    value = c(8, 6),
                                    continent = rep("Europe & Central Asia", 2))
caps2 <- rbind(caps1, serbia_and_montenegro)
caps2 %>% 
  count(continent)

# Create a map-shaped window:
df3[df3$country == "Congo Democratic Republic", "country"] <- "Congo (Kinshasa)"
spdf <- joinCountryData2Map(df3, joinCode = "NAME", nameJoinColumn = "country")
#sort(spdf$NAME)
#setdiff(df3$country, spdf$country)

# Obtain the plot:
mapDevice("x11")
mapCountryData(spdf, nameColumnToPlot = "value", catMethod = "fixedWidth", 
               mapTitle = "", colourPalette	= "white2Black")
text(caps2$long, caps2$lat - 1, caps2$value, pos = 3, cex = 0.5)
