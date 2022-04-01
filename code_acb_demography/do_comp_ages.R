# Load needed packages:
library(readr)
library(countrycode)
library(dplyr)
library(lubridate)
library(ggplot2)

# Set working directory:
setwd("")

# Read games' data:
df0 <- read_csv("code_acb_demography/output/2020_2021/stats_games_2021.csv")

# Check teams' names:
# Tenerife has two names. We merge them and leave only one:
df0 %>% 
  distinct(Team) %>%
  arrange(Team)

df00 <- df0 %>%
  select(Player, CombinID, Team, MP) %>%
  mutate(Team = plyr::mapvalues(Team, from = "IberostarTfe", to = "LenovoTenerife")) %>%
  filter(Player != "Equipo") 

df00 %>% 
  distinct(Team) %>%
  arrange(Team)

# Compute minutes played by players:
df00_mp <- df00 %>%
  mutate(MP = ifelse(MP == "0", "00:00", MP)) %>%
  group_by(CombinID, Player, Team) %>%
  summarise(MP = round(sum(as.numeric(as.period(ms(MP), unit = "sec"))) / 60)) %>%
  ungroup()

# Identify those players who have played more than 360 minutess
df01_mp <- df00_mp %>%
  mutate(more_than_360_min = ifelse(MP > 360, "yes", "no"))

# Count games played by players: 
df01_gp <- df00 %>%
  count(CombinID, Player, name = "num_games")

# Count the amount of players in each time:
df1 <- df00 %>%
  select(-MP) %>%
  distinct(Team, CombinID, .keep_all = TRUE)
df1 %>%
  count(Team) %>%
  arrange(-n)

# Read players' data:
df0_pl <- read_csv("code/output/2020_2021/players_data_2021.csv")

# Translate nationalities to English:
# There is one player with no scraped information:
# http://www.acb.com/jugador/temporada-a-temporada/id/30000998
df0_pl[is.na(df0_pl$Nationality), "Nationality"] <- "Dinamarca"

nat_spa <- sort(unique(df0_pl$Nationality))
nat_eng_tr <- countryname(nat_spa)

nat_spa[is.na(nat_eng_tr)]
nat_eng_tr[is.na(nat_eng_tr)] <- c("Belarus", "USA", "UK", "Czech Republic", 
                                   "Dominican Republic", "Sudan")

df00_pl <- df0_pl %>%
  mutate(Nationality = plyr::mapvalues(Nationality, 
                                       from = nat_spa,
                                       to = nat_eng_tr))
# Quino Colom is an Andorran player who has always played for Spain.
df00_pl$Nationality[df00_pl$Player == "J. Colom"] <- "Spain"

df1_pl <- df00_pl %>%
  select(CombinID, Nationality)

# Join players' nationalities and minutes played:
df2 <- left_join(df1, df1_pl, by = "CombinID")

df2_copy <- df2 %>% 
  mutate(Nationality = ifelse(Nationality == "Spain", "Spaniard", "Foreigner"))

df3 <- left_join(df2_copy, df01_mp, by = c("Player", "CombinID", "Team")) %>%
  arrange(Team, Nationality)

df4 <- df3 %>%
  select(CombinID, Player, Nationality, more_than_360_min)

info_pl <- df00_pl %>%
  select(CombinID, Date_birth)

df5 <- left_join(df4, info_pl)
# Age at first of june 2021.
df6 <- df5 %>%
  mutate(Date_birth = dmy(Date_birth),
         Age = interval(Date_birth, "2021-06-01") %>% as.numeric("years")) %>%
  filter(more_than_360_min == "yes") %>%
  select(Nationality, Age)
range(df6$Age)

df7 <- df6 %>% 
  group_by(Nationality, group_age = cut(Age, breaks = seq(19, 43, by = 4))) %>% 
  summarise(value = n()) %>%
  arrange(as.numeric(group_age)) %>%
  ungroup() %>%
  arrange(Nationality)

ggplot(df7, aes(group_age, Nationality)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = value), size = 7, color = "white") + 
  scale_fill_gradient(low = "grey", high = "black") +
  labs(x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        legend.title = element_blank())
#ggsave("comp_ages.eps", width = 8)  
