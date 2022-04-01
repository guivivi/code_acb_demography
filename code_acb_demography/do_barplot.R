# Load needed packages:
library(readr)
library(countrycode)
library(dplyr)
library(lubridate)
library(tidyr) # uncount
library(ggplot2)
library(ggpol) # facet_share

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

# There are players who played in more than one team:
df1 %>% janitor::get_dupes(CombinID)
df1 %>% janitor::get_dupes(CombinID) %>% distinct(Player)
#df1[df1$CombinID == "20212348", "Player"] <- paste(df1[df1$CombinID == "20212348", "Player"],
#                                                   "Manzanares", sep = "_")

# Read players' data:
df0_pl <- read_csv("code_acb_demography/output/2020_2021/players_data_2021.csv")
#df0_pl[238, "Player"]$Player <- paste(df0_pl[238, "Player"]$Player, "Manzanares", sep = "_")

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

# Nationalities by teams:
df3 <- df2 %>%
  count(Team, Nationality)

df4 <- df3 %>%
  mutate(Nationality = ifelse(Nationality == "Spain", "Spaniard", "Foreigner")) %>%
  group_by(Team, Nationality) %>%
  summarise(count = sum(n)) %>%
  ungroup()

df5 <- df4 %>%
  spread(key = Nationality, value = count) %>%
  mutate(ratio = Foreigner / Spaniard) %>%
  gather(key = category, value = count, -Team, -ratio) %>%
  # uncount will fail if there is NAs
  uncount(count) %>% 
  # calculate y position (needs to negative for one gender factor level)
  group_by(Team, category) %>%
  mutate(y = sequence(n())) %>%
  mutate(y = ifelse(category == "Foreigner", -y, y)) %>%
  ungroup() %>%
  mutate_if(is.character, factor)

# Join the information of more than 360 minutes played: 
# --- Data to check results with http://www.acb.com/club/index/temporada_id/2020/edicion_id/960
review <- left_join(df2_copy, df01_mp, by = c("Player", "CombinID", "Team")) %>%
  arrange(Team, Nationality)
# ---

df_aux_copy <- left_join(df2_copy, df01_mp, by = c("Player", "CombinID", "Team")) %>%
  arrange(Team, Nationality)

df_aux <- left_join(df2_copy, df01_mp, by = c("Player", "CombinID", "Team")) %>%
  arrange(Team, Nationality) %>%
  select(-Player, -CombinID)

df_aux1 <- df_aux %>%
  filter(Nationality == "Foreigner") %>%
  arrange(Team, Nationality, more_than_360_min)

df_aux2 <- df_aux %>%
  filter(Nationality == "Spaniard") %>%
  arrange(Team, Nationality, more_than_360_min)

df_aux3 <- rbind(df_aux1, df_aux2) %>%
  select(more_than_360_min)

df6 <- cbind(df5, df_aux3)

df6 %>% count(Team) %>% arrange(n)

andorra <- df_aux_copy %>%
  filter(Team == "MoraBancAnd") %>%
  select(CombinID, Player, Nationality) %>%
  rename(Nationality_group = 3)
andorra1 <- left_join(andorra, df00_pl) %>%
  select(CombinID, Player, Nationality_group, Nationality)
View(andorra1)

df_aux1 %>% count(more_than_360_min) # Foreigners.
df_aux2 %>% count(more_than_360_min) # Spaniards.

# Arrange by teams with most foreigners:
teams_foreig <- df4 %>% 
  filter(Nationality == "Foreigner") %>% 
  arrange(desc(count)) %>% 
  pull(Team)

df5 <- df5 %>%
  arrange(match(Team, teams_foreig)) %>%
  mutate(Team = as.character(Team)) %>%
  mutate(Team = factor(Team, levels = unique(df5$Team)))

df6 <- df6 %>%
  arrange(match(Team, teams_foreig)) %>%
  mutate(Team = as.character(Team)) %>%
  mutate(Team = factor(Team, levels = unique(df5$Team)))

ggplot() +
  geom_point(data = df6,
             mapping = aes(x = Team,
                           y = y,
                           color = more_than_360_min),
             shape = 19,
             size = 4) +
  facet_share(~ category, 
              dir = "h", 
              scales = "free",
              reverse_num = TRUE) +
  coord_flip() + 
  scale_color_manual(values = c("black", "grey")) +
  scale_x_discrete(name = NULL, 
                   limits = rev(levels(df5$Team))) +
  scale_y_continuous(breaks = seq(min(df5$y), max(df5$y), 1)) +
  theme_bw() +
  #theme_classic() +
  #theme_test() +
  theme(strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)) +
  labs(y = NULL)
ggsave("barplot.eps", width = 19, height = 10)

df6_prop <- df6 %>%
  count(Team, category) %>%
  group_by(Team) %>%
  mutate(prop = (n / sum(n)) * 100)  %>%
  ungroup() 
