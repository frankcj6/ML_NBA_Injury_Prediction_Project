# scrape websites to get schedule & results (match information and results)
# setwd('~/MDML_Final_Project')

library(tidyverse)
library(lubridate)
library(rvest)


## Schedule & Results
year <- c('2017','2018','2019')

playoff_startDate <- ymd('2017-04-15','2018-04-14','2019-04-13')

month <- c('october','november','december',
           'january','february','march','april','may','june')

col_names <- read_html('https://www.basketball-reference.com/leagues/NBA_2017_games.html') %>% 
  html_nodes("table#schedule > thead > tr > th") %>% 
  html_attr("data-stat")   
col_names <- c("game_id", col_names)

for(i in seq(year)){
  for(j in seq(month)){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                  year[i],"_games-",month[j],".html")
    webpage <- read_html(url)
    
    dates <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>% 
      html_text()
    dates <- dates[dates != "Playoffs"]
    
    game_id <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    
    data <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = length(col_names) - 2, byrow = TRUE)
    
    month_df <- as.data.frame(cbind(game_id, dates, data), 
                              stringsAsFactors = FALSE)
    
    names(month_df) <- col_names
    
    # change columns to the correct types
    month_df$visitor_pts <- as.numeric(month_df$visitor_pts)
    month_df$home_pts    <- as.numeric(month_df$home_pts)
    month_df$attendance  <- as.numeric(gsub(",", "", 
                                            month_df$attendance))
    month_df$date_game   <- mdy(month_df$date_game)
    
    # add column to indicate if regular season or playoff
    month_df$game_type <- with(month_df,
                               ifelse(date_game>=playoff_startDate[i],
                                      "Playoff","Regular"))
    # drop boxscore column
    month_df$box_score_text <- NULL
    
    if(i==1&j==1){
      tbl <- month_df
    }else{
      tbl <- rbind(tbl,month_df)
    }
    
  }
}

write_csv(tbl,'data/processed/game.csv')


