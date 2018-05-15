
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

# get game number for each team
dat_2016 <- get_game_num(dat_2016)
dat_2017 <- get_game_num(dat_2017)


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

dat$dataset_player <- dat$venue.r.h <- dat$`venue_(r/h)`<- dat$minutes <- dat$year_team <- dat$month_team <- 
  dat$data_set <- dat$opponent <- dat$ot1 <- dat$ot2 <- dat$ot3 <- dat$ot4 <- dat$to_to <- 
  dat$starter_1 <- dat$starter_2 <- dat$starter_3 <- dat$starter_4 <- dat$starter_5 <- dat$opening_odds <- 
  dat$opening_spread <- dat$opening_total <- dat$movements <- dat$closing_odds <- dat$moneyline <- dat$halftime <- 
  dat$box_score <- dat$odds <- dat$year<- dat$moneyline <- dat$fg_per <- dat$venue <- NULL

rm(dat_full, dat_player, fan_data)


# calculate usage rate for each player's game and compare to data usage rate
# usage rate 100 * ((FGA + 0.44 * FTA + TOV) * (Tm MP / 5)) / (MP * (Tm FGA + 0.44 * Tm FTA + Tm TOV)).
# tm MP -233.26
# tm FGA
# tm FTA
# tm TOV
dat$temp_usage <- round(((dat$fga_player + (dat$fta_player)*0.44 + dat$to_player)*
                           ((dat$min_team) / 5))/(dat$min_player*(dat$fga_team + (dat$fta_team)*0.44 + dat$to_team))*100, 2)

dat$vi <- ((dat$pts_player)*(dat$tot_player)*(dat$a_player))^(0.333)
dat$per <- ((dat$fg_player*85.91) + (dat$st_player*53.87) + (dat$`3p_player`*51.575) + (dat$ft_player)*46.846 + 
              (dat$bl_player*39.190) + (dat$or_player*39.19) + (dat$a_player*34.677) + (dat$dr_player)*14.707 -
              (dat$pf_player*17.174) - (dat$fta_player - dat$ft_player)*20.091 - (dat$fga_player - dat$fg_player)*39.19  -
              (dat$to_player)*53.897)*(1/dat$min_player)
dat$ppm <- (dat$pts_player/dat$min_player)
dat$ftapm <- (dat$fta_player/dat$min_player)

dat <- dat[dat$usage_rate < 90, ]

dat$ppu <- round((dat$pts_player/dat$usage_rate),2)


# Usage rate, a.k.a., usage percentage is an estimate of the percentage of team plays used by a player while he was on the floor. By balancing usage rates and the varying offensive ratings of the five players on the court, a team can achieve optimal offensive output. The stats show that, for all players, as the player uses more possessions, his efficiency decreases. What defines a superstar, in Dean Oliver‘s statistical analysis, is that he can shoulder a larger proportion of a team’s possessions with only a relatively small drop in efficiency. Meanwhile, the opposite is also true: players perform more efficiently when they are asked to use fewer of their team’s possessions. As a result, the greater burden on the superstar means that supporting players maintain low usage rates, allowing them to operate closer to their peak efficiency.

# get offense and defensive efficiency
# field goals attempted - offensive rebounds + turnovers + (0.4 x free throws attempted) = 
# total number of possessions for the season. then points over total possessions
# need to estimate this by dividing by minutes.
dat$oeff_player <- dat$pts_player/(dat$fga_player - dat$or_player + dat$to_player + (0.4*dat$fta_player))

# remove playoffs from data
dat <- dat[!grepl('Playoffs', dat$dataset_team),]
dat$dataset_team <- NULL
# -----------
# for the blog: explore what determines a usage rate, fantasy points, abd salary
# most underperforming vs overperforming - salary (prior) vs fantasy points (posterior)

# teams, players with highest salary
temp_dat_team <- dat %>%
  filter(!is.na(teams)) %>%
  group_by(teams) %>%
  summarise(sum_pts = round(sum(pts_player, na.rm = TRUE), 2),
            sum_salary = round(sum(salary_dk, na.rm = TRUE), 2),
            sum_fan_pts = round(sum(fantasy_points_dk, na.rm = TRUE), 2),
            sum_usage = round(sum(usage_rate, na.rm = T), 2),
            sum_minutes = sum(min_team, na.rm = T))

temp_dat_player <- dat %>%
  filter(!is.na(teams)) %>%
  group_by(season, player) %>%
  summarise(mean_pts = round(mean(pts_player, na.rm = TRUE), 2),
            mean_salary = round(mean(salary_dk, na.rm = TRUE), 2),
            mean_fan_pts = round(mean(fantasy_points_dk, na.rm = TRUE), 2),
            mean_usage = round(mean(usage_rate, na.rm = T), 2),
            mean_minutes = mean(min_player, na.rm = T))

# look at first 10 games vs last 10 games
first_10_2016 <- dat[dat$game_num <= 10 & dat$season == "The 2016-2017 Season",]
first_10_2017 <- dat[dat$game_num <= 10 & dat$season == "The 2017-2018 Season",]
last_10_2016 <- dat[dat$game_num  >= 72 & dat$season == "The 2016-2017 Season",]
last_10_2017 <- dat[dat$game_num >= 72 & dat$season == "The 2017-2018 Season",]

# group by player for each data set and get stass 
first_group_16 <- first_10_2016 %>%
  filter(!is.na(player)) %>%
  group_by(player) %>%
  summarise(mean_min = round(mean(min_player, na.rm = TRUE), 2),
            mean_pts = round(mean(pts_player, na.rm = TRUE), 2),
            mean_salary = round(mean(salary_dk, na.rm = TRUE), 2),
            mean_fan_pts = round(mean(fantasy_points_dk, na.rm = TRUE),2),
            mean_usage_rate = round(mean(usage_rate, na.rm = TRUE), 2))
first_group_16$games <- 'first_10'

last_group_16 <- last_10_2016 %>%
  filter(!is.na(player)) %>%
  group_by(player) %>%
  summarise(mean_min = round(mean(min_player, na.rm = TRUE), 2),
            mean_pts = round(mean(pts_player, na.rm = TRUE), 2),
            mean_salary = round(mean(salary_dk, na.rm = TRUE), 2),
            mean_fan_pts = round(mean(fantasy_points_dk, na.rm = TRUE),2),
            mean_usage_rate = round(mean(usage_rate, na.rm = TRUE), 2))
last_group_16$games <- 'last_10'


# join by player 
group_16 <- inner_join(first_group_16, last_group_16, by = 'player')

# rename columns to identify the first10 and last 10 games stats.
names(group_16) <- gsub('.y','_last10', names(group_16), fixed = TRUE)
names(group_16) <- gsub('.x','_first10', names(group_16), fixed = TRUE)

# get the difference between first and last game for each statistic
group_16$mean_min_diff <- 

# look at who has the biggest diff between salary and fantasy points, both overall ad over time











