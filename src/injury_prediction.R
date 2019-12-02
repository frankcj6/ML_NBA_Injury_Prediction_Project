# Try to predict injury for NBA players
# setwd('~/MDML_Final_Project')

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ROCR)
set.seed(2048)

### CONVERT DATA TO DESIRED FORMAT ###

player_injury <- read_csv('data/processed/player_injury.csv') %>% 
  mutate(season = case_when(date > '2016-10-01' & date < '2017-07-01' ~ '2016/2017',
                            date > '2017-10-01' & date < '2018-07-01' ~ '2017/2018'),
         season = as.factor(season),
         month = case_when(month(date) >= 10 & month(date) <= 12 ~ (month(date)-9),
                           month(date) >= 1 & month(date) <= 6 ~ (month(date)+3)))


# Mean of stats before this match
player_injury <- player_injury %>% 
  group_by(season, player) %>% 
  mutate(num_match_before = row_number()-1,
         mean_starter_before = (cumsum(starters)-starters)/(row_number()-1),
         mean_min_before = (cumsum(mp)-mp)/(row_number()-1),
         mean_pts_before = (cumsum(pts)-pts)/(row_number()-1),
         mean_trb_before = (cumsum(trb)-trb)/(row_number()-1),
         mean_pf_before = (cumsum(pf)-pf)/(row_number()-1),
         mean_off_rtg_before = (cumsum(off_rtg)-off_rtg)/(row_number()-1),
         mean_def_rtg_before = (cumsum(def_rtg)-def_rtg)/(row_number()-1)) %>% 
  arrange(season, player)

# stats of last one match
player_injury <- player_injury %>% 
  group_by(season, player) %>%
  mutate(starter_last = lag(starters),
         min_last = lag(mp),
         pts_last = lag(pts),
         trb_last = lag(trb),
         pf_last = lag(pf),
         off_rtg_last = lag(off_rtg),
         def_rtg_last = lag(def_rtg),
         home_last = lag(home))


# select relevant variables
player_injury_rel <- player_injury %>% 
  select(season, player, date, injury, home, starters, names(player_injury)[42:ncol(player_injury)]) %>% 
  filter(is.na(home_last)==FALSE)

### DEVELOP PREDICTION MODEL ###
player_injury_train <- player_injury_rel %>% sample_frac(.8)
player_injury_test <- player_injury_rel %>% anti_join(player_injury_train)

formula <- "injury~home+starters+month+num_match_before+mean_starter_before+mean_min_before+
mean_pts_before+mean_trb_before+mean_pf_before+mean_off_rtg_before+mean_def_rtg_before+
starter_last+min_last+pts_last+trb_last+pf_last+off_rtg_last+def_rtg_last+home_last"

model_injury <- glm(formula, data = player_injury_train, family = 'binomial')
summary(model_injury)

predicted_prob = predict(model_injury, player_injury_test, type = 'response')
perf <- prediction(predicted_prob, player_injury_test$injury)
auc <- performance(perf, "auc")
auc <- auc@y.values[[1]]
auc

### RECOMMENDATION ###


