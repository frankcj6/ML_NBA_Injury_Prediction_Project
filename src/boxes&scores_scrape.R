# scrape websites to get boxes & scores (player stats)
# setwd('~/MDML_Final_Project')

library(tidyverse)
library(lubridate)
library(rvest)


## list of links
year <- c('2017','2018')

month <- c('october','november','december',
           'january','february','march','april','may','june')

date_team <- vector()

for(i in seq(year)){
  for(j in seq(month)){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                  year[i],"_games-",month[j],".html")
    webpage <- read_html(url)
    
    csk <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th.left") %>% 
      html_attr("csk")
    date_team <- append(date_team,csk)
  }
}


## function to scrap each game
extract_game <- function(csk){
  
  ## Boxes & Scores
  url <- paste0("https://www.basketball-reference.com/boxscores/",csk,".html")
  webpage <- read_html(url)

  # home and away abb
  abb <- webpage %>%
    html_nodes(xpath='//span//strong') %>%
    html_text()
  away <- abb[1]
  home <- abb[2]
    
  
  teams <- webpage %>%
    html_nodes(xpath='//a[@itemprop="name"]') %>%
    html_text()
  
  date <- webpage %>%
    html_nodes(xpath='//h1') %>%
    html_text() %>%
    stringr::str_split_fixed(', ',2)
  date <- mdy(date[2])
  
  
  ## team 1: away
  ## basic
  # column names of Basic Box Score Stat
  col_b1 <- webpage %>% 
    html_nodes(paste0("table#box-",away,"-game-basic > thead > tr > th")) %>% 
    html_attr("data-stat")
  col_b1 <- col_b1[4:length(col_b1)]
  
  # list of players
  player1 <- webpage %>% 
    html_nodes(paste0("table#box-",away,"-game-basic > tbody > tr > th > a")) %>% 
    html_text()
  
  # data only of the table
  data_b1 <- webpage %>% 
    html_nodes(paste0("table#box-",away,"-game-basic > tbody > tr > td.right")) %>% 
    html_text()
  data_b1 <- as.data.frame(matrix(data_b1,ncol=length(col_b1),byrow=T),
                           stringsAsFactors = FALSE)
  names(data_b1) <- col_b1
  
  # add players
  player1 <- tibble(player=player1)
  
  basic1 <- qpcR:::cbind.na(player1,data_b1)
  
  # starters or reserves
  temp1 <- webpage %>%
    html_nodes(paste0("table#box-",away,"-game-basic > tbody > tr")) %>%
    html_text()
  n_starters1 <- grep('Reserves',temp1)-1
  starters <- c(rep(1,n_starters1),rep(0,nrow(player1)-n_starters1))
  
  basic1 <- cbind(starters,basic1)
  
  
  ## advanced
  # column names of Advanced Box Score Stat
  col_a1 <- webpage %>% 
    html_nodes(paste0("table#box-",away,"-game-advanced > thead > tr > th")) %>% 
    html_attr("data-stat")
  col_a1 <- col_a1[4:length(col_a1)]
  
  # data only of the table
  data_a1 <- webpage %>% 
    html_nodes(paste0("table#box-",away,"-game-advanced > tbody > tr > td.right")) %>% 
    html_text()
  data_a1 <- as.data.frame(matrix(data_a1,ncol=length(col_a1),byrow=T),
                           stringsAsFactors = FALSE)
  names(data_a1) <- col_a1
  
  # add players and team total
  adv1 <- qpcR:::cbind.na(player1,data_a1)
  
  # starters or reserves
  adv1 <- cbind(starters,adv1)
  
  # join basic and advanced, add team and date
  team1 <- left_join(basic1,adv1,by=c('player','mp','starters')) %>%
    mutate(team=teams[1],date=date,home=0)
  
  
  ## team 2: home 
  ## basic
  # column names of Basic Box Score Stat
  col_b2 <- webpage %>% 
    html_nodes(paste0("table#box-",home,"-game-basic > thead > tr > th")) %>% 
    html_attr("data-stat")
  col_b2 <- col_b2[4:length(col_b2)]
  
  # list of players
  player2 <- webpage %>% 
    html_nodes(paste0("table#box-",home,"-game-basic > tbody > tr > th > a")) %>% 
    html_text()
  
  # data only of the table
  data_b2 <- webpage %>% 
    html_nodes(paste0("table#box-",home,"-game-basic > tbody > tr > td.right")) %>% 
    html_text()
  data_b2 <- as.data.frame(matrix(data_b2,ncol=length(col_b2),byrow=T),
                           stringsAsFactors = FALSE)
  names(data_b2) <- col_b2
  
  # add players and team total
  player2 <- tibble(player=player2)
  
  basic2 <- qpcR:::cbind.na(player2,data_b2)
  
  # starters or reserves
  temp2 <- webpage %>%
    html_nodes(paste0("table#box-",home,"-game-basic > tbody > tr")) %>%
    html_text()
  n_starters2 <- grep('Reserves',temp2)-1
  starters <- c(rep(1,n_starters2),rep(0,nrow(player2)-n_starters2))
  
  basic2 <- cbind(starters,basic2)
  
  
  ## advanced
  # column names of Advanced Box Score Stat
  col_a2 <- webpage %>% 
    html_nodes(paste0("table#box-",home,"-game-advanced > thead > tr > th")) %>% 
    html_attr("data-stat")
  col_a2 <- col_a2[4:length(col_a2)]
  
  # data only of the table
  data_a2 <- webpage %>% 
    html_nodes(paste0("table#box-",home,"-game-advanced > tbody > tr > td.right")) %>% 
    html_text()
  data_a2 <- as.data.frame(matrix(data_a2,ncol=length(col_a2),byrow=T),
                           stringsAsFactors = FALSE)
  names(data_a2) <- col_a2
  
  # add players and team total
  adv2 <- qpcR:::cbind.na(player2,data_a2)
  
  # starters or reserves
  adv2 <- cbind(starters,adv2)
  
  # join basic and advanced, add team and date
  team2 <- left_join(basic2,adv2,by=c('player','mp','starters')) %>%
    mutate(team=teams[2],date=date,home=1)
  
  # combine 2 teams
  game <-rbind(team1,team2)
  
  return(game)
}

# get data from all games
tbl <- lapply(date_team,extract_game)
tbl <- bind_rows(tbl)

write_csv(tbl,'data/processed/player.csv')











# ## one sample page for testing
# ## Boxes & Scores
# url <- "https://www.basketball-reference.com/boxscores/201910220LAC.html"
# webpage <- read_html(url)
# 
# teams <- webpage %>%
#   html_nodes(xpath='//a[@itemprop="name"]') %>%
#   html_text()
# 
# date <- webpage %>%
#   html_nodes(xpath='//h1') %>%
#   html_text() %>%
#   stringr::str_split_fixed(', ',2)
# date <- mdy(date[2])
# 
# 
# ## team 1
# ## basic
# # column names of Basic Box Score Stat
# col_b1 <- webpage %>% 
#   html_nodes("table#box-LAL-game-basic > thead > tr > th") %>% 
#   html_attr("data-stat")
# col_b1 <- col_b1[4:length(col_b1)]
# 
# # list of players
# player1 <- webpage %>% 
#   html_nodes("table#box-LAL-game-basic > tbody > tr > th > a") %>% 
#   html_text()
# 
# # data only of the table
# data_b1 <- webpage %>% 
#   html_nodes("table#box-LAL-game-basic > tbody > tr > td.right") %>% 
#   html_text()
# data_b1 <- as.data.frame(matrix(data_b1,ncol=length(col_b1),byrow=T),
#                          stringsAsFactors = FALSE)
# names(data_b1) <- col_b1
# 
# # add players and team total
# player1 <- tibble(player=player1)
# 
# basic1 <- qpcR:::cbind.na(player1,data_b1)
# 
# # starters or reserves
# temp1 <- webpage %>%
#   html_nodes("table#box-LAL-game-basic > tbody > tr") %>%
#   html_text()
# n_starters1 <- grep('Reserves',temp1)-1
# starters <- c(rep(1,n_starters1),rep(0,nrow(player1)-n_starters1))
# 
# basic1 <- cbind(starters,basic1)
# 
# 
# ## advanced
# # column names of Advanced Box Score Stat
# col_a1 <- webpage %>% 
#   html_nodes("table#box-LAL-game-advanced > thead > tr > th") %>% 
#   html_attr("data-stat")
# col_a1 <- col_a1[4:length(col_a1)]
# 
# # data only of the table
# data_a1 <- webpage %>% 
#   html_nodes("table#box-LAL-game-advanced > tbody > tr > td.right") %>% 
#   html_text()
# data_a1 <- as.data.frame(matrix(data_a1,ncol=length(col_a1),byrow=T),
#                          stringsAsFactors = FALSE)
# names(data_a1) <- col_a1
# 
# # add players and team total
# adv1 <- qpcR:::cbind.na(player1,data_a1)
# 
# # starters or reserves
# adv1 <- cbind(starters,adv1)
# 
# # join basic and advanced, add team and date
# team1 <- left_join(basic1,adv1,by=c('player','mp','starters')) %>%
#   mutate(team=teams[1],date=date)
# 
# # team_total <- webpage %>%
# #   html_nodes("table#box-LAL-game-basic > tfoot > tr > td.right") %>%
# #   html_text()
# # team_total <- c('team_toal',team_total)
# # 
# # basic <- rbind(basic,team_total)
# 
# 
# 
# ## team 2
# ## basic
# # column names of Basic Box Score Stat
# col_b2 <- webpage %>% 
#   html_nodes("table#box-LAC-game-basic > thead > tr > th") %>% 
#   html_attr("data-stat")
# col_b2 <- col_b2[4:length(col_b2)]
# 
# # list of players
# player2 <- webpage %>% 
#   html_nodes("table#box-LAL-game-basic > tbody > tr > th > a") %>% 
#   html_text()
# 
# # data only of the table
# data_b2 <- webpage %>% 
#   html_nodes("table#box-LAC-game-basic > tbody > tr > td.right") %>% 
#   html_text()
# data_b2 <- as.data.frame(matrix(data_b2,ncol=length(col_b2),byrow=T),
#                          stringsAsFactors = FALSE)
# names(data_b2) <- col_b2
# 
# # add players and team total
# player2 <- tibble(player=player2)
# 
# basic2 <- qpcR:::cbind.na(player2,data_b2)
# 
# # starters or reserves
# temp2 <- webpage %>%
#   html_nodes("table#box-LAC-game-basic > tbody > tr") %>%
#   html_text()
# n_starters2 <- grep('Reserves',temp2)-1
# starters <- c(rep(1,n_starters2),rep(0,nrow(player2)-n_starters2))
# 
# basic2 <- cbind(starters,basic2)
# 
# 
# ## advanced
# # column names of Advanced Box Score Stat
# col_a2 <- webpage %>% 
#   html_nodes("table#box-LAC-game-advanced > thead > tr > th") %>% 
#   html_attr("data-stat")
# col_a2 <- col_a2[4:length(col_a2)]
# 
# # data only of the table
# data_a2 <- webpage %>% 
#   html_nodes("table#box-LAC-game-advanced > tbody > tr > td.right") %>% 
#   html_text()
# data_a2 <- as.data.frame(matrix(data_a2,ncol=length(col_a2),byrow=T),
#                          stringsAsFactors = FALSE)
# names(data_a2) <- col_a2
# 
# # add players and team total
# adv2 <- qpcR:::cbind.na(player2,data_a2)
# 
# # starters or reserves
# adv2 <- cbind(starters,adv2)
# 
# # join basic and advanced, add team and date
# team2 <- left_join(basic2,adv2,by=c('player','mp','starters')) %>%
#   mutate(team=teams[2],date=date)
# 
# # combine 2 teams
# game <-rbind(team1,team2)
# 
