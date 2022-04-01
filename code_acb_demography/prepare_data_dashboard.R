# Load needed packages:
library(readr)
library(countrycode)
library(dplyr)
library(lubridate)
library(tidyr)

# Set working directory:
setwd("")

# Read games' data:
df0 <- read_csv("code_acb_demography/output/2020_2021/stats_games_2021.csv")

df00 <- df0 %>%
  mutate(Team = plyr::mapvalues(Team, from = "IberostarTfe", to = "LenovoTenerife")) %>%
  filter(Player != "Equipo") %>%
  select(GS, CombinID, Player, Team, MP, PTS, TwoP, TwoPA, ThreeP, ThreePA, FT, FTA, TRB, DRB,        
         ORB, AST, STL, TOV, Counteratt, BLKfv, BLKag, Dunks, PF, PFrv, PlusMinus, PIR)

# Compute games played:
df00_gp <- df00 %>%
  group_by(CombinID, Player, Team) %>% 
  summarise(GP = n()) %>%
  ungroup()

# Compute minutes played:
df00_mp <- df00 %>%
  mutate(MP = ifelse(MP == "0", "00:00", MP)) %>%
  group_by(CombinID, Player, Team) %>%
  summarise(MP = round(sum(as.numeric(as.period(ms(MP), unit = "sec"))) / 60)) %>%
  ungroup()

# Compute accumulated stats:
df00_stats <- df00 %>%
  select(-MP) %>%
  group_by(CombinID, Player, Team) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup()

df1 <- left_join(df00_gp, df00_mp, by = c("CombinID", "Player", "Team"))
df11 <- left_join(df1, df00_stats, by = c("CombinID", "Player", "Team"))

df2_team <- df11 %>%
  select(CombinID, Team)

df2 <- df11 %>%
  mutate(TwoPPerc = ifelse(TwoPA == 0, 0, round((TwoP / TwoPA) * 100, 1)), .after = TwoPA) %>%
  mutate(ThreePPerc = ifelse(ThreePA == 0, 0, round((ThreeP / ThreePA) * 100, 1)), .after = ThreePA) %>%
  mutate(FTPerc = ifelse(FTA == 0, 0, round((FT / FTA) * 100, 1)), .after = FTA) %>%
  arrange(Team, Player) #%>%
  #select(-CombinID)

# Read players' data:
df0_player <- read_csv("code_acb_demography/output/2020_2021/players_data_2021.csv")

# Translate nationalities to English:
# There is one player with no scraped information:
# http://www.acb.com/jugador/temporada-a-temporada/id/30000998
df0_player[is.na(df0_player$Nationality), "Position"]$Position <- "Base"
df0_player[is.na(df0_player$Nationality), "Height"]$Height <- "196"
df0_player[is.na(df0_player$Nationality), "Date_birth"]$Date_birth <- "06/02/2003"
df0_player[is.na(df0_player$Nationality), "Nationality"]$"Nationality" <- "Dinamarca"

nat_spa <- sort(unique(df0_player$Nationality))
nat_eng_tr <- countryname(nat_spa)

nat_spa[is.na(nat_eng_tr)]
nat_eng_tr[is.na(nat_eng_tr)] <- c("Belarus", "USA", "UK", "Czech Republic", 
                                   "Dominican Republic", "Sudan")

df00_player <- df0_player %>%
  mutate(Nationality = plyr::mapvalues(Nationality, 
                                       from = nat_spa,
                                       to = nat_eng_tr))
# Quino Colom is an Andorran player who has always played for Spain.
df00_player$Nationality[df00_player$Player == "J. Colom"] <- "Spain"

# Add team:
df1_player <- left_join(df00_player, df2_team, by = "CombinID") 

df2_player <- df1_player %>%
  select(Team, everything()) %>%
  rowwise() %>%
  mutate(Website_picture = paste0('<img src="', Website_picture, '"></img>')) %>%
  mutate(Website_player = paste0("<a href='", Website_player, "' target='_blank'>", Website_player, "</a>")) %>%
  ungroup() %>%
  distinct(CombinID, Player, .keep_all = TRUE) 

df3_player <- df2_player %>%
  mutate(Position = plyr::mapvalues(Position,
                                    from = unique(df2_player$Position)[1:5],
                                    to = c("Center", "Point guard", "Small forward", 
                                           "Shooting guard", "Power forward"))) %>%
  select(-Website_picture) %>%
  rename(Country = Nationality) %>%
  mutate(Nationality = ifelse(Country == "Spain", "Spaniard", "Foreigner"))
  #arrange(Player)

df31_player <- df3_player %>%
  select(CombinID, Player, Country, Nationality)

df32_player <- df3_player %>%
  select(Player, Country, Nationality, everything(), -Team, -CombinID)
write_csv(df32_player, "code_acb_demography/output/acb_2021_info_players.csv")

df2_def <- left_join(df2, df31_player, by = c("CombinID", "Player")) %>%
  select(Player, Country, Nationality, everything(), -CombinID)
write_csv(df2_def, "code_acb_demography/output/acb_2021_stats_players.csv")
