
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

# read in fantasy player data
fan_2017 <- read.csv('data/player_fantasy_2016_2017.csv', stringsAsFactors = F)
fan_2018 <- read.csv('data/player_fantasy_2018.csv', stringsAsFactors = F)

# combine data 
fan_data <- rbind(fan_2017, fan_2018)

# clean data 
names(fan_data) <- tolower(names(fan_data))

colnames(fan_data) <- gsub(' ', '_', colnames(fan_data))

# convert date
fan_data$date <- as.Date(fan_data$date, format = '%m/%d/%Y')

# get year
fan_data$year <- as.factor(format(fan_data$date, format = '%Y'))

# create a month variable 
fan_data$month <- month(as.POSIXlt(fan_data$date))
  
# change name to join on
names(fan_data)[4] <- 'teams'
rm(fan_2017, fan_2018)

# usage rate 100 * ((FGA + 0.44 * FTA + TOV) * (Tm MP / 5)) / (MP * (Tm FGA + 0.44 * Tm FTA + Tm TOV)).
# tm MP -233.26
# tm FGA
# tm FTA
# tm TOV

# read in player stats
temp_2017 <- read_csv('data/player_stat_2016_17.csv')
temp_2018 <- read_csv('data/player_stat_2018_reg.csv')

# combine 
dat_player <- rbind(temp_2017,
                    temp_2018)

rm(temp_2016, temp_2017, temp_2018)

# clean up cols
names(dat_player) <- tolower(names(dat_player))
names(dat_player) <- gsub(' ', '_', names(dat_player))
names(dat_player)[5] <- 'teams'
names(dat_player)[3] <- 'player'

# convert date
dat_player$date <- as.Date(dat_player$date, format = '%m/%d/%Y')

# get year
dat_player$year <- as.factor(format(dat_player$date, format = '%Y'))

# create a month variable 
dat_player$month <- month(as.POSIXlt(dat_player$date))


# inner join by date, teams, player
dat <- inner_join(dat_player, fan_data, by = c('date', 'player', 'teams'))

# read in team data
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
dat_2016$season <- 'The 2016-2017 Season'
dat_2017$season <- 'The 2017-2018 Season'

# combine data
dat_full <- rbind(dat_2016,
                  dat_2017)

rm(dat_2016, dat_2017)

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

# left join data 
dat <- left_join(dat, dat_full, by = c('date', 'teams'))


# recode to get starters 
names(dat) <- ifelse(names(dat) == 'starting_lineups', 'starter_1', 
                     ifelse(names(dat) == 'x37', 'starter_2',
                            ifelse(names(dat) == 'x38', 'starter_3',
                                   ifelse(names(dat) == 'x39', 'starter_4',
                                          ifelse(names(dat) == 'x40', 'starter_5', names(dat))))))

# create variable that indicates if player started that game
dat$starting_team <- ifelse(dat$player == dat$starter_1 | dat$player == dat$starter_2 | 
                              dat$player == dat$starter_3 | dat$player == dat$starter_4 | dat$player == dat$starter_5, 
                            'yes', 'no')




names(dat) <- gsub('.x', '_player',names(dat), fixed = TRUE)
names(dat) <- gsub('.y', '_team',names(dat), fixed = TRUE)
names(dat) <- gsub('_(r/h)', '', names(dat), fixed = TRUE)

dat$dataset_player <- dat$venue.r.h <- dat$minutes <- dat$year_team <- dat$month_team <- dat$dataset_team <- 
  dat$data_set <- dat$opponent <- dat$ot1 <- dat$ot2 <- dat$ot3 <- dat$ot4 <- dat$to_to <- dat$starter_1 <- 
  dat$starter_2 <- dat$starter_3 <- dat$starter_4 <- dat$starter_5 <- dat$opening_odds <- dat$opening_spread <-
  dat$opening_total <- dat$movements <- dat$closing_odds <- dat$moneyline <- dat$halftime <- dat$box_score <- 
  dat$odds <- dat$season <- dat$year<- dat$moneyline <- dat$fg_per <- NULL

rm(dat_full, dat_player, fan_data)
