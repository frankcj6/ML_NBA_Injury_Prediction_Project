# Home and away matches winning probability analysis
# setwd('~/MDML_Final_Project')

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ROCR)

### READ AND JOIN DATA ###

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

### EXPLORATORY RESEARCH ###

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

# exploratory research 5: any difference in strategy?
p5.1 <- ggplot(plot.data, aes(x=as.factor(home), y=fg3a_per_fga_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p5.2 <- ggplot(plot.data, aes(x=as.factor(home), y=fta_per_fga_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p5.3 <- ggplot(plot.data, aes(x=as.factor(home), y=tov.total)) +
  geom_boxplot() +
  labs(x='home')
p5.4 <- ggplot(plot.data, aes(x=as.factor(home), y=usg_pct.mean)) +
  geom_boxplot() +
  labs(x='home')
p5 <- ggarrange(p5.1,p5.2,p5.3,p5.4, ncol=2, nrow=2)
ggsave(plot=p5, file='figures/p5_strategy.png', width = 10, height = 8)

# exploratory research 6: any difference in energy?
starter_mp <- player %>%
  filter(starters==1) %>% 
  group_by(date,team) %>% 
  summarise(starter_min = mean(mp))
game_home <- game_home %>% 
  left_join(starter_mp)
game_away <- game_away %>% 
  left_join(starter_mp)
plot.data = rbind(game_home, game_away)

mean(game_home$starter_min)
mean(game_away$starter_min)
p6 <- ggplot(plot.data, aes(x=as.factor(home), y=starter_min)) +
  geom_boxplot() +
  labs(x='home')
ggsave(plot=p6, file='figures/p6_starter_energy.png', width = 5, height = 5)


### DEVELOP LOGISTIC REGRESSION MODEL ###

# formula and two models
formula_home = "win~attendance+
                fg_pct.mean+fg3_pct.mean+
                orb.total+drb.total+
                pf.total+stl.total+blk.total+def_rtg.mean+
                starter_min"
formula_away = "win~distance+attendance+
                fg_pct.mean+fg3_pct.mean+
                orb.total+drb.total+
                pf.total+stl.total+blk.total+def_rtg.mean+
                starter_min"

model_home_train <- game_home %>% slice(1:round(0.8*nrow(game_home)))
model_home_test <- game_home %>% slice(round(0.8*nrow(game_home))+1:nrow(game_home))
model_home <- glm(formula_home, data = model_home_train, family = 'binomial')
summary(model_home)

model_away_train <- game_away %>% slice(1:round(0.8*nrow(game_home)))
model_away_test <- game_away %>% slice(round(0.8*nrow(game_home))+1:nrow(game_home))
model_away <- glm(formula_away, data = model_away_train, family = 'binomial')
summary(model_away)

# performance of two models
predicted_prob1 = predict(model_home, model_home_test, type = 'response')
perf <- prediction(predicted_prob1, model_home_test$win)
auc1 <- performance(perf, "auc")
auc1 <- auc1@y.values[[1]]
auc1

predicted_prob2 = predict(model_away, model_away_test, type = 'response')
perf <- prediction(predicted_prob2, model_away_test$win)
auc2 <- performance(perf, "auc")
auc2 <- auc2@y.values[[1]]
auc2

# visualize winning probability
model_home <- glm(formula_home, data = game_home, family = 'binomial')
game_home$winning_prob <- predict(model_home, game_home, type = 'response')
model_away <- glm(formula_away, data = game_away, family = 'binomial')
game_away$winning_prob <- predict(model_away, game_away, type = 'response')
plot.data <- rbind(game_home, game_away) %>% 
  group_by(team, home) %>% 
  summarise(win_prob_mean = mean(winning_prob, na.rm=TRUE))
p_wp <- ggplot(plot.data, aes(x=team, y=win_prob_mean, color = as.factor(home))) +
  geom_point() + 
  scale_x_discrete(labels=c(1:30)) +
  guides(color = guide_legend(title = "home"))
  
ggsave(plot=p_wp, file='figures/winning_prob.png', width = 10, height = 5)


