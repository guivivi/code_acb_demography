# Load needed packages:
library(readr)
library(dplyr)
library(countrycode)
library(BAwiR)
library(ggplot2)

# Set working directory:
setwd("")

# Load players' data from the last three seasons collected:
df0 <- read_csv("code_acb_demography/output/2018_2019/players_data_1819.csv")
df00 <- df0 %>%
  select(Player, Nationality) %>%
  rename(Player.x = 1) %>%
  mutate(Season = "2018-2019") 
df00[is.na(df00$Nationality), "Nationality"] <- "Serbia"

df1 <- read_csv("code_acb_demography/output/2019_2020/players_data_1920.csv")
# Quino Colom is an Andorran player who has always played for Spain.
df1$Nationality[df1$Player == "J. Colom"] <- "Spain"
df11 <- df1 %>%
  select(Player, Nationality) %>%
  rename(Player.x = 1) %>%
  mutate(Season = "2019-2020") %>%
  mutate_if(is.factor, as.character)
df11[is.na(df11$Nationality), "Nationality"] <- "Chile"
df11[df11$Nationality == "", "Nationality"] <- "Senegal"

df2 <- read_csv("code_acb_demography/output/2020_2021/players_data_2021.csv")
# There are two players with the same name (rows 238 and 269).
df2[238, "Player"]$Player <- paste(df2[238, "Player"]$Player, "Manzanares", sep = "_")
df2$Nationality[df2$Player == "J. Colom"] <- "Spain"
df22 <- df2 %>%
  select(Player, Nationality) %>%
  rename(Player.x = 1) %>%
  mutate(Season = "2020-2021") %>%
  mutate_if(is.factor, as.character)
df22[is.na(df22$Nationality), "Nationality"] <- "Dinamarca"

df3 <- rbind(df22, df11, df00)

nat_spa <- sort(unique(df3$Nationality))
nat_eng_tr <- countryname(nat_spa)

nat_spa[is.na(nat_eng_tr)]
nat_eng_tr[is.na(nat_eng_tr)] <- c("Belarus", "United States", "Guyana",
                                   "United Kingdom", "Czech Rep.",
                                   "Dominican Rep.", "Germany", "S. Sudan")

df4 <- df3 %>%
  mutate(Nationality = plyr::mapvalues(Nationality, 
                                       from = nat_spa,
                                       to = nat_eng_tr))


# Join with the previous seasons:
load(url("http://www.uv.es/vivigui/softw/data_app_acb.RData"))

data_acb <- rbind(df4, data_app_acb)

title <- ""
get_pop_pyramid(data_acb, title, "eng") + 
  scale_fill_grey(start = 0.5)
#ggsave("pyramid.eps", width = 12, height = 7)
