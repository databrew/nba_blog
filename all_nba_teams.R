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

# read in player data and fantasy data
dat_player <- read_csv('data/player_data/nba-season-player-feed_april.csv')
dat_fan <- read_csv('data/fantasy_data/dfs_in_season_april.csv')

# convert data 

# clean column names
names(dat_player) <- c('data_set', 'date', 'player_name', 'positition',
                       'team', 'opp_team', 'venue', 'min', 'fg', 'fga',
                       'three_point', 'three_point_att', 'ft', 'fta', 
                       'or', 'dr', 'tot_r', 'assists', ' personal_fouls', 
                       'steals', 'turn_overs', ' blocks', 'points')

# remove unneded columns from fantasy data
dat_fan$FANDUEL <- dat_fan$YAHOO <- dat_fan$FANDUEL_1 <- dat_fan$FANDUEL_2 <- dat_fan$YAHOO_1 <- dat_fan$YAHOO_2 <- 
  dat_fan$`GAME ID` <-  dat_fan$`PLAYER ID` <- NULL

names(dat_fan) <- c('data_set', 'date', 'player_name', 'team', 'opp_team', 'starter',
                    'venue', 'min', 'usage_rate', 'days_rest', 'posistion' ,'salary',
                    'fan_points')

# combine data set by date and player name and team
dat <- inner_join(dat_player, dat_fan, by = c('data_set', 'date', 'player_name'))



