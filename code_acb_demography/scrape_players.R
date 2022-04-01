# Load needed packages:
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(stringi)
library(purrr)
library(readr)

# SCRAPE PLAYERS' DATA:
scrape_new_acb_players <- function(code, name_player, user_email, user_agent_goo) {
  
  url_base <- "http://www.acb.com/jugador/temporada-a-temporada/id/"
  url_link <- paste(url_base, code, sep = "")
  
  link_content <- GET(url_link, 
                      user_agent(str_c(user_agent_goo, R.version$version.string, sep = ", ")),
                      add_headers(from = user_email))
  
  if (link_content$status == 404) {
    stop("URL not found. Please check if it exists.")
  }else{
    url_html <- read_html(url_link)
    url_data <- url_html %>%
      html_nodes(xpath = './/div[@class="f-l-a-100 contenedora_datos_basicos"]') %>%
      html_text()
    
    if (length(url_data) == 0) {
      frame_player <- data.frame(CombinID = code,
                                 Player = name_player, 
                                 Position = NA,
                                 Height = NA,
                                 Date_birth = NA,
                                 Nationality = NA,
                                 Website_player = url_link,
                                 Website_picture = NA)       
    }else{
      data_player <- strsplit(url_data, "\n")[[1]]
      data_player1 <- stri_replace_all_charclass(data_player, "\\p{WHITE_SPACE}", "")
      data_player2 <- data_player1[data_player1 != ""]
      
      url_data_sec <- url_html %>%
        html_nodes(xpath = './/div[@class="f-l-a-100 contenedora_datos_secundarios"]') %>%
        html_text()
      
      data_player_sec <- strsplit(url_data_sec, "\n")[[1]]
      data_player_sec1 <- stri_replace_all_charclass(data_player_sec, "\\p{WHITE_SPACE}", "")
      data_player_sec2 <- data_player_sec1[data_player_sec1 != ""]
      
      url_foto <- url_html %>%
        html_nodes("img") %>% 
        map(xml_attrs) %>% 
        map_df(~as.list(.)) %>%
        filter(alt == "Foto Jugador" & !is.na(style)) %>%
        pull(src)
      url_foto1 <- paste("http:", url_foto, sep = "")
      
      frame_player <- data.frame(CombinID = code,
                                 Player = name_player, 
                                 # Remove accents:
                                 Position = stri_trans_general(gsub(".*:", "", 
                                                                    data_player2[4]), "Latin-ASCII"),
                                 Height = gsub(".*:", "", gsub("m", "", data_player2[5])),
                                 Date_birth = gsub("\\(.*", "", gsub(".*:", "", data_player_sec2[3])),
                                 Nationality = gsub(".*,", "", data_player_sec2[2]),
                                 Website_player = url_link,
                                 Website_picture = url_foto1)  
    }
    
    return(data_player = frame_player) 
  }
}

# Update these parameters accordingly:
user_email <- ""
user_agent_goo <- ""

# code <- "30000004"
stats_games <- list.files(pattern = "*.csv", full.names = TRUE) %>%
  map_df(~read_csv(.)) %>%
  arrange(Day)

write_csv(stats_games, "stats_games.csv")

df0 <- stats_games %>% 
  select(Player, CombinID) %>% 
  filter(Player != "Equipo") %>%
  distinct(CombinID, .keep_all = TRUE)
# There are two players named J. Fernandez in season 2020-2021.
#df1 <- df0 %>%
#  count(Player) %>%
#  arrange(-n)

players_code <- df0$CombinID
players_name <- df0$Player

players_data <- data.frame()
for (i in 1:length(players_code)) {
  cat("ITERATION:", i, "\n")
  cat("PLAYER CODE:", players_code[i], "\n")
  
  iter <- scrape_new_acb_players(players_code[i], players_name[i], user_email, user_agent_goo)
  
  players_data <- rbind(players_data, iter)
  
  Sys.sleep(5)
}

write_csv(players_data, "players_data_1819.csv")
