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
for(i in seq(2, nrow(dat_game), 2)){
  
  # get new data
  temp_dat <- dat_game[i:(i-1),]
  game_max <- max(dat_game$f[i:(i-1)])
  game_min <- min(dat_game$f[i:(i-1)])
  
  temp_dat$real_spread <-ifelse(temp_dat$f == game_max, (game_max - game_min)*(-1),
                                 game_max - game_min)
  
  dat_game[i:(i-1),] <- temp_dat

}


# recode f to points_scored
colnames(dat_game)[colnames(dat_game) == 'f'] <- 'points_scored'

# get game id
dat_game$game_id <- rep(1:(nrow(dat_game)/2), each=2)

# ------------------------------------------------------------------------------------------------
# by game analysis

# group by game number and get spread diff, spread fac, over under diff, over under fac
dat_game <- get_by_game(dat_game)

# keep some columns
dat_game <- dat_game[, c('date_home', 'teams_home', 'teams_away','points_scored_home', 'points_scored_away', 
                         'opening_over_under_home', 'closing_over_under_home','opening_spread_home', 'closing_spread_home', 'real_total_home')]


 # ------------------------------------------------------------------------------------------------
# by team analysis




# make a column for spread numeric error
# make a column for over under right or wrong
# make a column for over under numeric error
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
