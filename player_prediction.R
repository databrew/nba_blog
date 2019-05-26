# Player usage and points prediction


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

# read in data containing fantasy stats, player stats, and team stats
dat <- readRDS('data/dat_mod.rda')

# slim down data and keep only the basics for now
keepers <- 