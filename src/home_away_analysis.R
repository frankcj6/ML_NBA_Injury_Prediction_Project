# Home and away matches winning probability analysis
# setwd('~/MDML_Final_Project')

library(tidyverse)
library(ggplot2)
library(ggpubr)

### read and join data ###

game_away <- read_csv('data/processed/game_away.csv')
game_home <- read_csv('data/processed/game_home.csv')
player_game_level <- read_csv('data/processed/player_game_level.csv')

game_away <- game_away %>% 
  rename(date = date_game,
         team = team_name)
game_home <- game_home %>% 
  rename(date = date_game,
         team = team_name)

game_away <- game_away %>% 
  left_join(player_game_level)
game_home <- game_home %>% 
  left_join(player_game_level)

### exploratory research ###

# Is there substancial difference in scores between home and visitor teams?
# Plot win or loss for home and visitor teams respectively with point size
plot.data <- rbind(game_home, game_away)
py <- ggplot(plot.data, aes(x=team, y=win, color=as.factor(home))) +
  geom_count(alpha=0.5) + 
  scale_size_area(max_size = 20) +
  scale_x_discrete(labels = c(1:30)) +
  guides(color = guide_legend(title = "home"))
ggsave(plot=py,file='figures/win_loss_plot.png', width = 10, height = 5)

# exploratory research 1: relationship with distance and attendance of audiance?
p1.1 <- ggplot(game_away, aes(x=as.factor(win), y=distance)) +
  geom_boxplot() +
  labs(x='win')
p1.2 <- ggplot(game_away, aes(x=as.factor(win), y=attendance)) +
  geom_boxplot() +
  labs(x='win')
p1 <- ggarrange(p1.1,p1.2)
ggsave(plot=p1,file='figures/p1_distance_attendance_plot.png', width = 10, height = 5)

# exploratory research 2: any difference in field goal efficiency?
p2.1 <- ggplot(plot.data, aes(x=as.factor(home), y=fg_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p2.2 <- ggplot(plot.data, aes(x=as.factor(home), y=fg3_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p2.3 <- ggplot(plot.data, aes(x=as.factor(home), y=ts_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p2.4 <- ggplot(plot.data, aes(x=as.factor(home), y=efg_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p2 <- ggarrange(p2.1,p2.2,p2.3,p2.4)
ggsave(plot=p2,file='figures/p2_field_goal_efficiency.png', width = 10, height = 8)

# exploratory research 3: any difference in rebonds?
p3.1 <- ggplot(plot.data, aes(x=as.factor(home), y=orb.total)) +
  geom_boxplot() +
  labs(x='home')
p3.2 <- ggplot(plot.data, aes(x=as.factor(home), y=drb.total)) +
  geom_boxplot() +
  labs(x='home')
p3 <- ggarrange(p3.1,p3.2)
ggsave(plot=p3,file='figures/p3_rebounds.png', width = 10, height = 5)

# exploratory research 4: any difference in defensive performance 
# (personal fouls, steals, blocks, defensive rating)?
p4.1 <- ggplot(plot.data, aes(x=as.factor(home), y=pf.total)) +
  geom_boxplot() +
  labs(x='home')
p4.2 <- ggplot(plot.data, aes(x=as.factor(home), y=stl.total)) +
  geom_boxplot() +
  labs(x='home')
p4.3 <- ggplot(plot.data, aes(x=as.factor(home), y=blk.total)) +
  geom_boxplot() +
  labs(x='home')
p4.4 <- ggplot(plot.data, aes(x=as.factor(home), y=def_rtg.mean)) +
  geom_boxplot() +
  labs(x='home')
p4 <- ggarrange(p4.1,p4.2,p4.3,p4.4, ncol=2, nrow=2)
ggsave(plot=p4, file='figures/p4_defensive_performance.png', width = 10, height = 5)

# exploratory research 4: any difference in defensive performance 
# (personal fouls, steals, blocks, defensive rating)?
p4.1 <- ggplot(plot.data, aes(x=as.factor(home), y=pf.total)) +
  geom_boxplot() +
  labs(x='home')
p4.2 <- ggplot(plot.data, aes(x=as.factor(home), y=stl.total)) +
  geom_boxplot() +
  labs(x='home')
p4.3 <- ggplot(plot.data, aes(x=as.factor(home), y=blk.total)) +
  geom_boxplot() +
  labs(x='home')
p4.4 <- ggplot(plot.data, aes(x=as.factor(home), y=def_rtg.mean)) +
  geom_boxplot() +
  labs(x='home')
p4 <- ggarrange(p4.1,p4.2,p4.3,p4.4, ncol=2, nrow=2)
ggsave(plot=p4, file='figures/p4_defensive_performance.png', width = 10, height = 8)

# exploratory research 4: any difference in offensive performance 
# (personal fouls, steals, blocks, defensive rating)?





