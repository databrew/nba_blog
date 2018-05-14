##########
# LOAD LIBRARIES and read in data
##########
library(broom)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(doParallel)
library(caret)
library(glmnet)
library(reshape2)
library(plotly)
library(scales)
library(MASS)
registerDoParallel()
# source functions script 
source('functions.R')

# read in data
dat_2015 <- read_csv('data/team_stat_2015_16.csv')
dat_2016 <- read_csv('data/team_stat_2016_17.csv')
dat_2017 <- read_csv('data/team_stat_2018_reg.csv')

# rempve WEEL variable from dat_2017
dat_2017$WEEK <- NULL

# rename columns that specify players with "X#"
colnames(dat_2017) <- gsub('X38', 'X37', colnames(dat_2017))
colnames(dat_2017) <- gsub('X39', 'X38', colnames(dat_2017))
colnames(dat_2017) <- gsub('X40', 'X39', colnames(dat_2017))
colnames(dat_2017) <- gsub('X41', 'X40', colnames(dat_2017))

# create an indicatr for season before combining 
dat_2015$season <- 'The 2015-2016 Season'
dat_2016$season <- 'The 2016-2017 Season'
dat_2017$season <- 'The 2017-2018 Season'

# combine data
dat_full <- rbind(dat_2015, 
                      dat_2016,
                      dat_2017)

rm(dat_2015, dat_2016, dat_2017)

# clean column names 
colnames(dat_full) <- tolower(colnames(dat_full))

colnames(dat_full) <- gsub(' ', '_', colnames(dat_full))

# convert date
dat_full$date <- as.Date(dat_full$date, format = '%m/%d/%Y')

# get year
dat_full$year <- as.factor(format(dat_full$date, format = '%Y'))

# create a month variable 
dat_full$month <- month(as.POSIXlt(dat_full$date))

# get percentages for fg, 3p and ft
dat_full$fg_per <- round(dat_full$fg/dat_full$fga, 2)


# loop through and add up score for each game to get real total
dat_full$real_total <- NA
for(i in seq(1, nrow(dat_full), 2)) {
  dat_full$real_total[i:(i+1)] <- sum(dat_full$f[i], dat_full$f[i+1])
  print(i)
}

# ideas: errors and percent correct.  what teams are most predictable and not. what players? 
# get variables for looking at team predictability

# get columns
dat_game <- dat_full[, c('date','year','month', 'teams','venue', 'opening_odds',
                         'opening_spread', 'closing_odds','odds', 'real_total', 'f',
                         'season')]

# where odds variable is na, fill with over_under, else 'the_line'
dat_game$odds <- ifelse(is.na(dat_game$odds), 'The Over/Under', 'The line')

# make opening odds have the over under for all cells (the max for each game)
# and assign min of closing odds  to the closing_line, and max to closing_over_under
dat_game$closing_spread <- NA
dat_game$closing_over_under <- NA

for(i in seq(2, nrow(dat_game), 2)){
  
  # get new data
  dat_game$opening_odds[i:(i-1)]  <- max(dat_game$opening_odds[i:(i-1)])
  dat_game$closing_spread[i:(i-1)] <- min(dat_game$closing_odds[i:(i-1)])
  dat_game$closing_over_under[i:(i-1)] <- max(dat_game$closing_odds[i:(i-1)])

  print(i)
}

# remove unneeded columns rename columns
dat_game$odds <- dat_game$closing_odds <-  NULL
colnames(dat_game) <- gsub('odds', 'over_under', colnames(dat_game))

# create underdog varliable 
dat_game$under_dog_mkt <- ifelse(dat_game$opening_spread < 0, 'favorite', 
                             ifelse(dat_game$opening_spread == 0, 'even','underdog'))

# ensure that closing spread has the same sign as opening spread 
dat_game$closing_spread <- ifelse(dat_game$under_dog_mkt =='favorite' & dat_game$closing_spread > 0, 
                                  dat_game$closing_spread*(-1),
                                  ifelse(dat_game$under_dog_mkt == 'underdog' & dat_game$closing_spread < 0,
                                         dat_game$closing_spread*(-1), dat_game$closing_spread))

# get real spread (already have real total for over under)
dat_game$real_spread <- NA
dat_game$final_result <- NA

for(i in seq(2, nrow(dat_game), 2)){
  
  # get new data
  temp_dat <- dat_game[i:(i-1),]
  game_max <- max(dat_game$f[i:(i-1)])
  game_min <- min(dat_game$f[i:(i-1)])
  
  temp_dat$real_spread <-ifelse(temp_dat$f == game_max, (game_max - game_min)*(-1),
                                 game_max - game_min)
  
  temp_dat$final_result <- ifelse(temp_dat$real_spread < 0, 'W', 'L')
  
  dat_game[i:(i-1),] <- temp_dat

}


# recode f to points_scored
colnames(dat_game)[colnames(dat_game) == 'f'] <- 'points_scored'

# get game id
dat_game$game_id <- rep(1:(nrow(dat_game)/2), each=2)


# ------------------------------------------------------------------------------------------------
# statistical tests

# H0: The The two variables are independent
# H1: The two variables are related.
# make contingency table 
# https://datascienceplus.com/chi-squared-test-in-r/

# first do chi squared test for home away, and win loss
dat_test <- dat_game[ , c('teams', 'venue', 'final_result')]
dat_test$final_result <- factor(dat_test$final_result, levels = c('W', 'L'))

table(dat_test$venue, dat_test$final_result)

# apply chi squared function
temp_chi <- get_chi_squared(dat_test)

# read in fantasy player data
fan_2017 <- read_csv('data/player_fantasy_2016_2017.csv')
# read in player data to see mean stats (t test, distributions, etc)
temp_2016 <- read_csv('data/player_stat_2015_16.csv')
temp_2017 <- read_csv('data/player_stat_2016_17.csv')
temp_2018 <- read_csv('data/player_stat_2018_reg.csv')

# combine data
dat_full <- rbind(temp_2016,
                  temp_2017,
                  temp_2018)


# clean and transform data
names(dat_full) <- tolower(names(dat_full))
names(dat_full) <- gsub(' ', '_', names(dat_full))
names(dat_full) <- gsub('_(r/h)', '', names(dat_full), fixed = T)


# regression of points on other variables
dat_mod <- dat_full[, c('pts', 'venue', 'min', 'fga', '3pa', 'fta', 
                        'or', 'dr', 'a', 'pf', 'st', 'to', 'bl')]

# linear regression
mod1 <- lm(pts ~ ., data = dat_mod)
summary(mod1)
mod_tab1 <- tidy(mod1)
mod_tab1$sig <- ifelse(mod_tab1$p.value <= 0.05, TRUE, FALSE)

# get Versatility Index Formula=[(PPG)*(RPG)*APG)]^(0.333), 
# PER: [ FGM x 85.910 + Steals x 53.897 + 3PTM x 51.757 + FTM x 46.845  + Blocks x 39.190 + Offensive_Reb	x 39.190 +
# Assists	x 34.677 + Defensive_Reb	x 14.707 - Foul x 17.174 - FT_Miss	x 20.091 - FG_Miss	x 39.190 - TO x 53.897 ] x (1 / Minutes).
dat_full$vi <- ((dat_full$pts)*(dat_full$tot)*(dat_full$a))^(0.333)
dat_full$per <- ((dat_full$fg*85.91) + (dat_full$st*53.87) + (dat_full$`3p`*51.575) + (dat_full$ft)*46.846 + 
                  (dat_full$bl*39.190) + (dat_full$or*39.19) + (dat_full$a*34.677) + (dat_full$dr)*14.707 -
                   (dat_full$pf*17.174) - (dat_full$fta - dat_full$ft)*20.091 - (dat_full$fga - dat_full$fg)*39.19  -
                     (dat_full$to)*53.897)*(1/dat_full$min)
dat_full$ppm <- (dat_full$pts/dat_full$min)
dat_full$ftapm <- (dat_full$fta/dat_full$min)


# group by player and get mean and std of pts, 3p, vi, per, ppm, ftapm,minutes, 
dat_player <- dat_full %>%
  group_by(player_full_name) %>%
  summarise(mean_pts = round(mean(pts, na.rm = T), 2),
            std_pts = round(sd(pts, na.rm = T),2),
            mean_ppm = round(mean(ppm, na.rm = T),2),
            std_ppm = round(sd(ppm, na.rm = T),2),
            mean_per = round(mean(per, na.rm = T),2),
            std_per = round(sd(per, na.rm = T),2),
            mean_vi = round(mean(vi, na.rm = T),2),
            std_vi = round(sd(vi, na.rm = T),2),
            mean_min = round(mean(min, na.rm = T),2),
            std_min = round(sd(min, na.rm = T),2),
            mean_fta = round(mean(fta, na.rm = T),2),
            std_fta = round(sd(fta, na.rm = T), 2), 
            games = n())

# visualize distributions
hist(dat_player$mean_ppm)
sd(dat_player$mean_pts)





# -----------------------------------------------------------------------------------------------
# - older code for visualizing


#1)
# # get real outcome
# dat_game$outcome <- ifelse(dat_game$real_spread < 0, 'Winner', 'Loser')
# 
# # make a column for right or wrong mkt prediction
# dat_game$win_loss_mkt_pred <- ifelse(dat_game$outcome == 'Winner' & dat_game$under_dog_mkt == 'underdog', 'bad', 
#                                      ifelse(dat_game$outcome == 'Winner' & dat_game$under_dog_mkt == 'favorite', 'good',
#                                             ifelse(dat_game$outcome == 'Loser' & dat_game$under_dog_mkt == 'underdog', 'good',
#                                                    ifelse(dat_game$outcome == 'Loser' & dat_game$under_dog_mkt == 'favorite', 'bad',
#                                                           ifelse(dat_game$under_dog_mkt == 'even', 'even', 'even')))))


#2)
# make sure that the opening odds and opening totals are consistent with the closing odds and totals.
# 
# # make a dataset that has one observation per game, with corresponding features
# dat_game <- get_by_game(dat_full)
# 
# # keep this columns - the opening odds and closing odds are actually the over unders
# keep_cols <- c('date_home','year_home','month_home' ,'teams_home','teams_away','pts_home', 'pts_away','real_total_home', 'opening_total_home', 'season_home')
# 
# # first plot the real total against the line. x axis date, y axis realtotal and over under
# dat <- dat_game[, keep_cols]
# 
# # turn month to character 
# dat <- dat[order(as.character(dat$month_home), decreasing = F),]
# dat$month_home <- month.abb[dat$month_home]
# 
# # get absolute value difference between real totla and mkt 
# dat$diff <- dat$real_total_home - dat$opening_total_home
# 
# # make data long  
# dat_long<- melt(dat, id.vars = c('date_home','year_home','month_home', 'teams_home','teams_away','pts_home', 'pts_away', 'season_home', 'diff'))
# 
# # rename columns
# colnames(dat_long) <- c('date', 'year', 'month', 'home_team', 'away_team', 'pts_home', 'pts_away','season', 'diff','variable', 'value')
# 
# # -------------------
# # plot - if discontinuous x axis you can facet_wrap year and have a plot with all data, just no trend or line. 
# # point, line plot by variable
# # plot all vm
# # revlevel variable if needed 
# dat_long$variable <- ifelse(grepl('real', dat_long$variable), 'Total points scored', 'Betting mkt')
# dat_long$variable <- factor(dat_long$variable, levels = c('Total points scored', 'Betting mkt'))
# 
# points_plot(dat_long, smooth_line = F, plot_title = 'All years',  x_time = 'years')
# 
# # break up by season
# dat_1 <- dat_long %>% filter(season %in% 'The 2015-2016 Season')
# dat_2 <- dat_long %>% filter(season %in% 'The 2016-2017 Season')
# dat_3 <- dat_long %>% filter(season %in% 'The 2017-2018 Season')
# 
# # plot each season
# points_plot(dat_1, smooth_line = T, plot_title = '2015-2016 season', x_time = 'months')
# points_plot(dat_2, smooth_line = T, plot_title = '2016-2017 season',  x_time = 'months')
# points_plot(dat_3, smooth_line = T, plot_title = '2017-2018 season',  x_time = 'months')
# 
# # histogram of each season with double distributions
# hist_plot_double(dat_1, plot_title = '2015-2016 season')
# hist_plot_double(dat_2, plot_title = '2016-2017 season') 
# hist_plot_double(dat_3, plot_title = '2017-2018 season')
# 
# # plot each histogram season
# hist_plot(dat_1, plot_title = '2015-2016 season')
# hist_plot(dat_2, plot_title = '2015-2016 season')
# hist_plot(dat_3, plot_title = '2015-2016 season')
