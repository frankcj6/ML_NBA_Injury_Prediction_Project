library(tidyverse)
library(lubridate)
library(rvest)

year <- "2018"
month <- "october"
url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
              "_games-", month, ".html")
webpage <- read_html(url)

col_names <- webpage %>% 
  html_nodes("table#schedule > thead > tr > th") %>% 
  html_attr("data-stat")    
col_names <- c("game_id", col_names)

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

month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
names(month_df) <- col_names

# change columns to the correct types
month_df$visitor_pts <- as.numeric(month_df$visitor_pts)
month_df$home_pts    <- as.numeric(month_df$home_pts)
month_df$attendance  <- as.numeric(gsub(",", "", month_df$attendance))
month_df$date_game   <- mdy(month_df$date_game)

# add column to indicate if regular season or playoff
playoff_startDate <- ymd("2018-04-14")
month_df$game_type <- with(month_df, ifelse(date_game >= playoff_startDate, 
                                "Playoff", "Regular"))

# drop boxscore column
month_df$box_score_text <- NULL





url <- "https://www.basketball-reference.com/boxscores/201910220LAC.html"
webpage <- read_html(url)

data <- webpage %>% 
  html_nodes("table#box-LAL-game-basic > tbody > tr > td") %>% 
  html_text()

