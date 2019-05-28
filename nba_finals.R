# this script will analyze raptors vs warriors finals game 

# load libraries
library(tidyverse)
library(ggplot2)
library(lattice)
library(pracma)
library(reshape2)
library(ggthemes)


# read in team datsa
dat_team <- read.csv('data/team_data/nba-season-team-feed_final.csv')
dat_player <- read.csv('data/player_data/nba-season-player-feed_finals.csv')

# remove and recode columns for team data
dat_team$LINE..MOVEMENT..1 <- dat_team$LINE..MOVEMENT..2 <- dat_team$LINE..MOVEMENT..3 <- dat_team$CLOSING.ODDS <- 
  dat_team$OPENING.ODDS <- dat_team$BOX.SCORE.URL <- dat_team$ODDS.URL <-  NULL
names(dat_team) <- trimws(gsub('URL', '', names(dat_team)), which = 'both')
names(dat_team) <- trimws(gsub('\n', ' ', names(dat_team)), which = 'both')
names(dat_team) <- gsub('X1', 'first_', names(dat_team))
names(dat_team) <- gsub('X2', 'second_', names(dat_team))
names(dat_team) <- gsub('X3', 'third_', names(dat_team))
names(dat_team) <- gsub('X4', 'fourth_', names(dat_team))
names(dat_team) <- gsub('STARTING.LINEUPS', 'player_1', names(dat_team))
names(dat_team) <- gsub('X.1', 'player_2', names(dat_team))
names(dat_team) <- gsub('X.2', 'player_3', names(dat_team))
names(dat_team) <- gsub('X.3', 'player_4', names(dat_team))
names(dat_team) <- gsub('X', 'player_5', names(dat_team))
names(dat_team) <- gsub('.', '_', names(dat_team), fixed = TRUE)
names(dat_team) <- tolower(names(dat_team))

# remove and recode columns for player data
names(dat_player) <- tolower(names(dat_player))
names(dat_player) <- gsub('.', '_', names(dat_player), fixed = TRUE)
names(dat_player)[names(dat_player) == 'venue__r_h_'] <- 'venue'

# convert date
dat_player$date <- as.Date(dat_player$date, format = '%m/%d/%Y')

# team match up - who plays better at home and away, who plays better with less rest, records vs east and west, who should start for the raptors
# which player gives team most of a boost - when player x plays well, how does team y do, and when player y plays bad, how does team x do.
# player match up - compare by position, plot rolling avg points throughout season (who is on uptrend and downtrend).

#########
# raptors vs opponents
#########

# convert date
dat_team$date <- as.Date(dat_team$date, format = '%m/%d/%Y')

# get team data
raps <- dat_team[dat_team$team == 'Toronto',]
wars <- dat_team[dat_team$team == 'Golden State',]

# get other team data
raps_opp <- dat_team[dat_team$team != 'Toronto',]
wars_opp <- dat_team[dat_team$team != 'Golden State',]

# join by game id 
raps_full <- inner_join(raps, raps_opp, by = 'game_id')
wars_full <- inner_join(wars, wars_opp, by = 'game_id')
rm(raps_opp, wars_opp, raps, wars)

# rename columns replaceing .x and .y
names(raps_full) <- gsub('.x', '_team', names(raps_full), fixed = TRUE)
names(raps_full) <- gsub('.y', '_opp', names(raps_full), fixed = TRUE)
names(wars_full) <- gsub('.x', '_team', names(wars_full), fixed = TRUE)
names(wars_full) <- gsub('.y', '_opp', names(wars_full), fixed = TRUE)

# get point diff
raps_full$point_diff <- raps_full$f_team - raps_full$f_opp
wars_full$point_diff <- wars_full$f_team - wars_full$f_opp

# bind rows
full_dat <- bind_rows(raps_full, wars_full)
rm(raps_full, wars_full)

#########
# histogram overlayed of points and points against and point diff
#########

ggplot(full_dat, aes(x = f_team, 
                     fill = team_team,
                     color = team_team)) + 
  geom_histogram(alpha = 0.8, 
                 binwidth = 2,
                 bins = 20) +
  labs(x = 'Total points per game',
       y = 'Counts',
       title = 'Distribution of points scored') +
  scale_fill_manual(name = '',
                    values = c('#006bb6', '#CD1141')) +
  scale_color_manual(name = '',
                     values = c('#FDB927', 'black'))

# points allowed
ggplot(full_dat, aes(x = f_opp, 
                     fill = team_team,
                     color = team_team)) + 
  geom_histogram(alpha = 0.8, 
                 binwidth = 2,
                 bins = 20) +
  labs(x = 'Total points allowed per game',
       y = 'Counts',
       title = 'Distribution of points allowed') +
  scale_fill_manual(name = '',
                    values = c('#006bb6', '#CD1141')) +
  scale_color_manual(name = '',
                     values = c('#FDB927', 'black'))

# points diff
ggplot(full_dat, aes(x = point_diff, 
                     fill = team_team,
                     color = team_team)) + 
  geom_histogram(alpha = 0.8, 
                 binwidth = 2,
                 bins = 20) +
  labs(x = 'Difference between points scored and allowed',
       y = 'Counts',
       title = 'Distribution of point differentials') +
  scale_fill_manual(name = '',
                    values = c('#006bb6', '#CD1141')) +
  scale_color_manual(name = '',
                     values = c('#FDB927', 'black')) +
  theme_classic()


##########
# moving avg of points and points againsgt and point diff
#########

# get mov avg
full_dat$mov_avg_points_team <- movavg(full_dat$f_team, n = 5, type = 's')
full_dat$mov_avg_points_opp <- movavg(full_dat$f_opp, n = 5, type = 's')
full_dat$mov_avg_points_diff <- movavg(full_dat$point_diff, n = 5, type = 's')

# plot mov avg
ggplot(full_dat, aes(date_team, 
                     mov_avg_points_team, 
                     group = team_team, 
                     color = team_team)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'loess') +
  scale_color_manual(name = '',
                     values = c('#006bb6', '#CD1141')) +
  ylim(c(90, 140)) +
  labs(x = 'Date',
       y = 'Moving avg of points scored') +
  scale_x_date(date_breaks = "2 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%B") +
  theme_pander()

# plot mov avg
ggplot(full_dat, aes(date_team, 
                     mov_avg_points_opp, 
                     group = team_team, 
                     color = team_team)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'loess') +
  scale_color_manual(name = '',
                     values = c('#006bb6', '#CD1141')) +
  ylim(c(90, 140)) +
  labs(x = 'Date',
       y = 'Moving avg of points allowed') +
  scale_x_date(date_breaks = "2 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%B") +
  theme_pander()

# plot mov avg
ggplot(full_dat, aes(date_team, 
                     mov_avg_points_diff, 
                     group = team_team, 
                     color = team_team)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'loess') +
  scale_color_manual(name = '',
                     values = c('#006bb6', '#CD1141')) +
  labs(x = 'Date',
       y = 'Moving avg of points allowed') +
  scale_x_date(date_breaks = "2 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%B") +
  theme_pander()
  


##########
# get performace vs east and west
##########

# read in teams.csv
con_teams <- read.csv('data/team_data/teams.csv')

# join with full_dat
full_dat <- inner_join(full_dat, con_teams, by = 'team_opp')

# group by team_team, filter out toronto and goldenstate in team_opp, and summarise 
# key variables
