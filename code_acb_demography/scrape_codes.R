# Load needed packages:
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(magrittr) # extract

# SCRAPE GAMES' CODES:
scrape_new_acb_code <- function(year = 2020, compet = 1, day = 1, user_email, user_agent_goo) {
  
  url_base <- "http://www.acb.com/resultados-clasificacion/ver/temporada_id"
  url_link <- paste(url_base, "/", year, "/competicion_id/", compet, "/jornada_numero/", day, sep = "")
  
  link_content <- GET(url_link, 
                      user_agent(str_c(user_agent_goo, R.version$version.string, sep = ", ")),
                      add_headers(from = user_email))
  
  if (link_content$status == 404) {
    stop("URL not found. Please check if it exists.")
  }else{
    url_html <- read_html(url_link)
    url_code0 <- url_html %>%
      html_nodes(xpath = './/article[@class="varios"]') 
    
    url_code <- url_code0 %>% 
      extract(seq(1, length(url_code0), 2)) %>%
      as.character() 
    
    url_code1 <- sapply(strsplit(url_code, '\\/id*.'), `[`, 2)
    url_code2 <- gsub('\\" .*', "", url_code1)
    
    return(url_code2)
  }
}

# Update these parameters accordingly:
user_email <- ""
user_agent_goo <- ""

#days <- 1:38 ; year <- 2020 # 2020-2021
#days <- 1:23 ; year <- 2019 # 2019-2020
days <- 1:34 ; year <- 2018 # 2018-2019

codes_day <- list()
for (i in 1:length(days)) {
  cat("ITERATION:", i, "\n")
  cat("DAY:", days[i], "\n")
  
  codes_day[[i]] <- scrape_new_acb_code(year = year, compet = 1, day = days[i], 
                                        user_email, user_agent_goo)
  
  Sys.sleep(5)
}
#unique(sapply(codes_day, length))
save(codes_day, file = "codes_day.RData")
