# preprocess data for further analysis
# setwd('~/MDML_Final_Project')

library(tidyverse)
library(lubridate)

### add distance to game data and split data into two datasets, for home and away ###
### matches respectively                                                          ###

# assumption: we use direct distance here, assume that players travel by airplanes
game <- read_csv('data/processed/game.csv')
game <- game %>% 
  select(-game_id, -game_start_time, -overtimes, -game_remarks) %>% 
  filter(date_game > '2016-10-25' & date_game < '2018-6-13')
distance.data = read.csv('data/raw/distance') %>% 
  mutate(team_name = as.character(team_name))

# write a function to calculate distance between two cities based on longtitude and latitude
calculate_distance <- function(long1, lati1, long2, lati2){
  R=3958.8
  dlon = (long2 - long1)*pi/180
  dlat = (lati2 - lati1)*pi/180
  lat1 = pi*lati1/180
  lat2 = pi*lati2/180
  a = sin(dlat/2)^2 + sin(dlon/2)^2 * cos(lat1) * cos(lat2)
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  R*c
}

# add win/loss variable
game <- game %>% 
  mutate(home_win = case_when(home_pts > visitor_pts ~ 1,
                              home_pts < visitor_pts ~ 0),
         visitor_win = 1-home_win)

# we develop two datasets for two models instead of developing one model
# data for home teams, distance = 0
game_home <- game %>% 
  mutate(team_name = home_team_name,
         distance = 0,
         pts = home_pts,
         win = home_win) %>% 
  select(date_game, team_name, pts, game_type, attendance, distance, win)

# data for visitor teams
game_away <- game
game_away <- game_away %>% 
  left_join(distance.data %>% rename(visitor_team_name = team_name)) %>% 
  rename(visitor_long=longtitude,
         visitor_lati=latitude) %>% 
  select(-abbreviation) %>% 
  left_join(distance.data %>% rename(home_team_name = team_name)) %>% 
  rename(home_long=longtitude,
         home_lati=latitude) %>% 
  mutate(distance = calculate_distance(visitor_long, visitor_lati,
                                       home_long, home_lati),
         team_name = visitor_team_name,
         pts = visitor_pts,
         win = visitor_win) %>% 
  select(date_game, team_name, pts, game_type, attendance, distance, win)

write_csv(game_home, path = 'data/processed/game_home.csv')
write_csv(game_away, path = 'data/processed/game_away.csv')

### convert player stats to game level for winning probability analysis ###
player <- read.csv('data/processed/player.csv')
player <- player %>% 
  mutate(mp = minute(ms(as.character(mp)))+second(ms(as.character(mp)))/60)
player <- as.tibble(player) %>% 
  filter(is.na(mp)==FALSE) %>% 
  mutate(player = as.character(player),
         team = as.character(team),
         date = as.Date(date))

player_game_level <- player %>% 
  group_by(date, team, home) %>% 
  summarise(fg.total = sum(fg, na.rm = TRUE),
            fga.total = sum(fga, na.rm = TRUE),
            fg_pct.mean = mean(fg_pct, na.rm = TRUE),
            fg3.total = sum(fg3, na.rm = TRUE),
            fg3a.total = sum(fg3a, na.rm = TRUE),
            fg3_pct.mean = mean(fg3_pct, na.rm = TRUE),
            ft.total = sum(ft, na.rm = TRUE),
            fta.total = sum(fta, na.rm = TRUE),
            ft_pct.mean = mean(ft_pct, na.rm = TRUE),
            orb.total = sum(orb, na.rm = TRUE),
            drb.total = sum(drb, na.rm = TRUE),
            trb.total = sum(trb, na.rm = TRUE),
            ast.total = sum(ast, na.rm = TRUE),
            stl.total = sum(stl, na.rm = TRUE),
            blk.total = sum(blk, na.rm = TRUE),
            tov.total = sum(tov, na.rm = TRUE),
            pf.total = sum(pf, na.rm = TRUE),
            pts.total = sum(pts, na.rm = TRUE),
            plus_minus.mean = mean(plus_minus, na.rm = TRUE),
            ts_pct.mean = mean(ts_pct, na.rm = TRUE),
            efg_pct.mean = mean(efg_pct, na.rm = TRUE),
            fg3a_per_fga_pct.mean = mean(fg3a_per_fga_pct, na.rm = TRUE),
            fta_per_fga_pct.mean = mean(fta_per_fga_pct, na.rm = TRUE),
            orb_pct.mean = mean(orb_pct, na.rm = TRUE),
            drb_pct.mean = mean(drb_pct, na.rm = TRUE),
            trb_pct.mean = mean(trb_pct, na.rm = TRUE),
            ast_pct.mean = mean(ast_pct, na.rm = TRUE),
            stl_pct.mean = mean(stl_pct, na.rm = TRUE),
            blk_pct.mean = mean(blk_pct, na.rm = TRUE),
            tov_pct.mean = mean(tov_pct, na.rm = TRUE),
            usg_pct.mean = mean(usg_pct, na.rm = TRUE),
            off_rtg.mean = mean(off_rtg, na.rm = TRUE),
            def_rtg.mean = mean(def_rtg, na.rm = TRUE))

write_csv(player_game_level, path = 'data/processed/player_game_level.csv')

### join injury data for injury analysis
injury_record <- read_csv('data/raw/injuries.csv')
injury_record <- injury_record %>% 
  filter(Date > "2016-10-25",
         is.na(Relinquised)==FALSE) %>% 
  select(-Acquired) %>% 
  rename(date = Date,
         team = Team,
         player = Relinquised)

# recode team name
injury_record <- injury_record %>% 
  mutate(team = case_when(team=='Hawks'~'Atlanta Hawks',
                          team=='Nets'~'Brooklyn Nets',
                          team=='Celtics'~'Boston Celtics',
                          team=='Hornets'~'Charlotte Hornets',
                          team=='Bulls'~'Chicago Bulls',
                          team=='Cavaliers'~'Cleveland Cavaliers',
                          team=='Mavericks'~'Dallas Mavericks',
                          team=='Nuggets'~'Denver Nuggets',
                          team=='Pistons'~'Detroit Pistons',
                          team=='Warriors'~'Golden State Warriors',
                          team=='Rockets'~'Houston Rockets',
                          team=='Pacers'~'Indiana Pacers',
                          team=='Clippers'~'Los Angeles Clippers',
                          team=='Lakers'~'Los Angeles Lakers',
                          team=='Grizzlies'~'Memphis Grizzlies',
                          team=='Heat'~'Miami Heat',
                          team=='Bucks'~'Milwaukee Bucks',
                          team=='Timberwolves'~'Minnesota Timberwolves',
                          team=='Pelicans'~'New Orleans Pelicans',
                          team=='Knicks'~'New York Knicks',
                          team=='Thunder'~'Oklahoma City Thunder',
                          team=='Magic'~'Orlando Magic',
                          team=='76ers'~'Philadelphia 76ers',
                          team=='Suns'~'Phoenix Suns',
                          team=='Blazers'~'Portland Trail Blazers',
                          team=='Kings'~'Sacramento Kings',
                          team=='Spurs'~'San Antonio Spurs',
                          team=='Raptors'~'Toronto Raptors',
                          team=='Jazz'~'Utah Jazz',
                          team=='Wizards'~'Washington Wizards'))

injury_record$player[grepl('/', x=injury_record$player)]
injury_record$player[grepl(')', x=injury_record$player)]

# recode player name
injury_record <- injury_record %>% 
  mutate(player = case_when(grepl('/', x=player)==TRUE~tail(strsplit(player,' / ')[[1]],1),
                            grepl('/', x=player)==FALSE~player))
injury_record <- injury_record %>%
  mutate(player = case_when(player == "(William) Tony Parker" ~ "Tony Parker",
                            player == "Ed Davis (a)" ~ "Ed Davis",
                            player == "Gerald Green (b)" ~ "Gerald Green",
                            player == "Gerald Henderson (b)" ~ "Gerald Henderson",
                            player == "John Collins (b)" ~ "John Collins",
                            player == "John Wall (a)" ~ "John Wall",
                            player == "Justin Hamilton (b)" ~ "Justin Hamilton",
                            player == "Reggie Jackson (b) " ~ "TReggie Jackson",
                            TRUE~player),
         injury=1) %>% 
  select(-Notes)

# join player and injury record
player_injury <- player %>% 
  left_join(injury_record) %>% 
  mutate(injury = case_when(is.na(injury)==TRUE ~ 0,
                            injury==1 ~ 1))
# adjust column order
player_injury <- player_injury %>% 
  select(date, team, player, home, injury, names(player_injury)[-c(2,37,38,39,40)])

write_csv(player_injury, path = 'data/processed/player_injury.csv')
